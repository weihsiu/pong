package pong.experiment

import cats.effect.IO

import scala.scalajs.js

object EffectTest extends js.JSApp {
  def effect1: IO[Unit] = {
    IO(println("world"))
  }
  def main(): Unit = {
    effect1.unsafeRunSync()
  }
}
