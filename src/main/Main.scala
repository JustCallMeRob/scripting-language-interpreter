package main

import scala.io.Source
import interpreter.Interpreter
import parser.Parser

object Main {
  def main(args: Array[String]) {
    val file = Source.fromFile("test.txt")
    val source = file.mkString

    val parser = new Parser
    parser.parseAll(parser.program, source) match {
      //if the parsing has been successful proceed with interpreting
      case parser.Success(r, n) => {
        println("Interpretation has begun:")
        val interpreter = new Interpreter(r)
        interpreter.run
        println("Interpretation finished")
      }
      //if parsing failed display the error
      case parser.Failure(msg, n) =>{
        println("Error: " + msg)
      }
    }
  }
}

