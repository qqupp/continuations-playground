package example

import data.continuation.basic.Continuation

object Goto extends App {

  val addressTable = new scala.collection.mutable.HashMap[String, Unit => Unit]()

  def LABEL(value: String) = Continuation[Unit, Unit]{ k => addressTable(value) = k; k()}
  def GOTO(value: String)= Continuation[Unit, Unit](k => addressTable(value)())
  def CONTINUE = Continuation[Unit, Unit](k => k())

  var counter = 0
  var game = 0

  val program = for {
    _ <- LABEL("START")
    _ =  counter = 1
    _ =  game += 1
    _ =  println(s"-- Round $game --")
    _ <- LABEL("INCREMENT")
    _ =  println(s" counter = $counter")
    _ =  counter += 1
    _ <- if (game > 2 && counter > 3)
           CONTINUE
         else if (counter <= 3)
           GOTO("INCREMENT")
         else
           GOTO("START")
    _ =  println("-- Bye Bye --")
  } yield ()

  program.run(x => x)

  /*
  output

    -- Round 1 --
     counter = 1
     counter = 2
     counter = 3
    -- Round 2 --
     counter = 1
     counter = 2
     counter = 3
    -- Round 3 --
     counter = 1
     counter = 2
     counter = 3
    -- Bye Bye --
   */

}
