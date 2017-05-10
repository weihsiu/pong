package pong

import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.atnos.eff.interpret._
import org.atnos.eff.all._
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import pong.IOEffect._
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js

object PongMain extends js.JSApp {
  object PressedKey extends Enumeration {
    type PressedKey = Value
    val LeftUp, LeftDown, RightUp, RightDown = Value
  }
  import PressedKey._
  case class GameConfig(width: Int, height: Int, paddleLength: Double, paddleVelocity: Double, paddleGap: Double, unit: Double)
  case class BallContext(position: (Double, Double), direction: Double, velocity: Double)
  case class GameContext(ball: BallContext, leftPaddle: Double, rightPaddle: Double)
  sealed trait GameAction[A]
  case class ReadKeyboard() extends GameAction[List[PressedKey]]
  case class UpdateContext(pressedKeys: List[PressedKey]) extends GameAction[Boolean]
  case class RenderScreen(flash: Boolean) extends GameAction[Unit]
  type _configReader[R] = Reader[GameConfig, ?] |= R
  type _contextState[R] = State[GameContext, ?] |= R
  type _gameAction[R] = GameAction |= R
  def readKeyboard[R : _gameAction]: Eff[R, List[PressedKey]] =
    Eff.send[GameAction, R, List[PressedKey]](ReadKeyboard())
  def updateContext[R : _gameAction](pressedKeys: List[PressedKey]): Eff[R, Boolean] =
    Eff.send[GameAction, R, Boolean](UpdateContext(pressedKeys))
  def renderScreen[R : _gameAction](flash: Boolean): Eff[R, Unit] =
    Eff.send[GameAction, R, Unit](RenderScreen(flash))
  def randomNum(min: Int, max: Int): IO[Int] = IO(new util.Random().nextInt(max - min + 1) + min)
  def gameLoop[R : _gameAction]: Eff[R, Unit] = for {
    ks <- readKeyboard
    f <- updateContext(ks)
    _ <- renderScreen(f)
  } yield ()
  def runGameLoop[R, U, A](c2d: CanvasRenderingContext2D, effects: Eff[R, A])(implicit
    m: Member.Aux[GameAction, R, U],
    r: _configReader[U],
    s: _contextState[U],
    io: _io[U]
  ): Eff[U, A] = {
    translate(effects)(new Translate[GameAction, U] {
      def apply[X](action: GameAction[X]): Eff[U, X] = action match {
        case ReadKeyboard() =>
          val pressedKeys = fromIO(List(KeyCode.A, KeyCode.Z, KeyCode.J, KeyCode.M)
            .map(kc => KeyboardStatus.keyPressed(kc))
            .sequence).map(_.zip(List(LeftUp, LeftDown, RightUp, RightDown)).filter(_._1).map(_._2))
          pressedKeys.flatMap(_.asInstanceOf[X].pure[Eff[U, ?]])
        case UpdateContext(ks) => for {
          config <- ask[U, GameConfig]
          context <- get[U, GameContext]
          quadrant <- fromIO(randomNum(0, 4))
          (newContext, flash) = PongLogic.updateContextPure(config, context, ks, quadrant)
          _ <- put[U, GameContext](newContext)
          r <- (flash.asInstanceOf[X]).pure[Eff[U, ?]]
        } yield r
        case RenderScreen(flash) => for {
          config <- ask[U, GameConfig]
          context <- get[U, GameContext]
          _ <- fromIO(PongLogic.renderScreenIO(c2d, config, context, flash))
          r <- (().asInstanceOf[X]).pure[Eff[U, ?]]
        } yield r
      }
    })
  }
  def runGame[R : _configReader : _contextState : _io, A](loop: Eff[R, A]): Eff[R, Unit] = for {
    ts <- fromIO(IO.async[Double](cb => dom.window.requestAnimationFrame(ts => cb(Right(ts)))))
    _ <- loop
    _ <- runGame(loop)
  } yield ()
  def main(): Unit = {
    val canvas = dom.document.querySelector("#canvas").asInstanceOf[dom.html.Canvas]
    val c2d = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    type Stack = Fx.fx4[GameAction, Reader[GameConfig, ?], State[GameContext, ?], IO]
    val config = GameConfig(canvas.width, canvas.height, 80, 3, 10, 15)
    val paddle = (config.height - config.paddleLength) / 2
    val context = GameContext(BallContext((config.width / 2, config.height / 2), PongLogic.directionInQuadrant(0), 2), paddle, paddle)
    val r = runGame(runGameLoop(c2d, gameLoop[Stack]))
      .runReader(config)
      .runState(context)
      .detach
    r unsafeRunAsync {
      case Left(e) => println(e)
      case Right(r) => println(r)
    }
  }
}