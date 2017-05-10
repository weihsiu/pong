package pong.experiment

import cats._
import cats.data._
import cats.effect.IO
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import pong.IOEffect._

import scala.scalajs.js

object EffTest extends js.JSApp {
  def main(): Unit = {
    type ReaderInt[A] = Reader[Int, A]
    type ReaderBoolean[A] = Reader[Boolean, A]
    type WriterString[A] = Writer[String, A]
    type _readerInt[R] = ReaderInt |= R
    type _readerBoolean[R] = ReaderBoolean |= R
    type _writerString[R] = WriterString |= R
    def program[R : _readerInt : _readerBoolean : _writerString : _io : _eval]: Eff[R, Int] = for {
      n <- ask[R, Int]
      b <- ask[R, Boolean]
      _ <- fromIO(IO(println(s"ask returned $n: $b")))
      _ <- tell("the required power is " + n)
      a <- delay(math.pow(2, n.toDouble).toInt)
      _ <- tell("the result is " + a)
      _ <- fromIO(IO(println("end")))
    } yield a
    type Stack = Fx.fx5[ReaderInt, ReaderBoolean, WriterString, IO, Eval]
    val result = program[Stack].runReader(true).runIO.runReader(6).runWriter.runEval.run
    println(result)
  }
}
