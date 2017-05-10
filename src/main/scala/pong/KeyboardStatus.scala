package pong

import cats.effect.IO
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent

object KeyboardStatus {
  var pressed = Set.empty[Int]
  dom.document.addEventListener("keydown", (e: KeyboardEvent) => pressed += e.keyCode, true)
  dom.document.addEventListener("keyup", (e: KeyboardEvent) => pressed -= e.keyCode, true)
  def keyPressed(keyCode: Int): IO[Boolean] = IO(pressed(keyCode))
}
