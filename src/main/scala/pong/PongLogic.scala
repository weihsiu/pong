package pong

import cats.effect.IO
import org.scalajs.dom.CanvasRenderingContext2D
import pong.PongMain._
import pong.PongMain.PressedKey._

import scala.scalajs.js

object PongLogic {
  def directionInQuadrant(quadrant: Int): Double = math.Pi / 4 + quadrant * math.Pi / 2
  def updateContextPure(config: GameConfig, context: GameContext, pressedKeys: List[PressedKey], quadrant: Int): (GameContext, Boolean) = {
    val unit = config.unit
    def updatePaddle(context: GameContext, pressedKey: PressedKey): GameContext = {
      def bounded(pos: Double): Double =
        if (pos < 0) 0
        else if (pos > config.height - config.paddleLength) config.height- config.paddleLength
        else pos
      pressedKey match {
        case LeftUp => context.copy(leftPaddle = bounded(context.leftPaddle - config.paddleVelocity))
        case LeftDown => context.copy(leftPaddle = bounded(context.leftPaddle + config.paddleVelocity))
        case RightUp => context.copy(rightPaddle = bounded(context.rightPaddle - config.paddleVelocity))
        case RightDown => context.copy(rightPaddle = bounded(context.rightPaddle + config.paddleVelocity))
      }
    }
    def updatePaddles(context: GameContext): GameContext = (context /: pressedKeys)(updatePaddle)
    def updateBall(context: GameContext): (GameContext, Boolean) = {
      def overlap(x1: Double, y1: Double, w1: Double, h1: Double, x2: Double, y2: Double, w2: Double, h2: Double): Boolean =
        x1 + w1 > x2 && y1 + h1 > y2 && x1 < x2 + w2 && y1 < y2 + h2
      def hitPaddle(x: Double, y: Double): Boolean =
        overlap(x, y, unit, unit, config.paddleGap, context.leftPaddle, unit, config.paddleLength) ||
        overlap(x, y, unit, unit, config.width - unit - config.paddleGap, context.rightPaddle, unit, config.paddleLength)
      val BallContext((x, y), d, v) = context.ball
      val newXY@(newX, newY) = (x + math.cos(d) * v, y + math.sin(d) * v)
      val (ball, flash) =
        if (hitPaddle(newX, newY)) (context.ball.copy(direction = math.Pi - d), false)
        else if (newX < 0 || newX >= config.width - unit) (context.ball.copy(position = (config.width / 2, config.height / 2), direction = directionInQuadrant(quadrant)), true)
        else if (newY < 0 || newY >= config.height - unit) (context.ball.copy(direction = -d), false)
        else (context.ball.copy(position = newXY), false)
      (context.copy(ball = ball), flash)
    }
    updateBall(updatePaddles(context))
  }
  def renderScreenIO(c2d: CanvasRenderingContext2D, config: GameConfig, context: GameContext, flash: Boolean): IO[Unit] = IO {
    val unit = config.unit
    def drawDivider = {
      c2d.setLineDash(js.Array(unit, unit))
      c2d.beginPath
      c2d.moveTo(config.width / 2, 0)
      c2d.lineTo(config.width / 2, config.height)
      c2d.stroke
    }
    def renderPaddle(x: Double, y: Double) = c2d.fillRect(x, y, unit, config.paddleLength)
    def renderBall(x: Double, y: Double) = c2d.fillRect(x, y, unit, unit)
    c2d.fillStyle = if (flash) "white" else "black"
    c2d.fillRect(0, 0, config.width, config.height)
    c2d.lineWidth = unit
    c2d.fillStyle = "white"
    c2d.strokeStyle = "white"
    drawDivider
    renderPaddle(config.paddleGap, context.leftPaddle)
    renderPaddle(config.width - unit - config.paddleGap, context.rightPaddle)
    renderBall(context.ball.position._1, context.ball.position._2)
  }
}
