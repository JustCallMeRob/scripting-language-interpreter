package parser

import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable.HashMap

//POSC(plain old scala classes)
case class Program(listOfFunctions:List[Function], listOfStatements:List[Statement])

case class Condition(leftPart: Expression, operator: String, rightPart: Expression)

trait Statement extends Positional
case class VariableDefinition(name: String, value: Expression) extends Statement
case class LoopBlock(times: Int, statements: List[Statement]) extends Statement
case class IfBlock(condition: Condition, trueBranch: List[Statement], falseBranch: List[Statement]) extends Statement
case class PrintLineStatement(value: Expression) extends Statement

class Expression extends Positional
case class Number(value: Int) extends Expression
case class Operator(var leftPart:Expression, operator:String, var rightPart: Expression ) extends Expression
case class Identifier(name:String) extends Expression

case class Function(functionName: String, argumentsMap: Map[String, Int], listOfStatements: List[Statement], returnValue: Expression)
case class FunctionCall(name: String, values: Map[String, Expression]) extends Expression with Statement

case class TypeChecker(ident: String) extends Statement

// Mutable HASHMAP where used because i wanted to implement variable scope but however this feature doesn't
// work and i wish i never started on it.
class Scope(val name: String, val parentScope: Scope) {
  var variables = new HashMap[String, Expression]
}


// Parser class which contains the rules for parsing, each rule will return an instance of a Parser with one of
// the POSC's defined above as its parameter. These will define the syntax of my programming language
class Parser extends StandardTokenParsers {

  lexical.delimiters += ("+", "-", "*", "/", "=", "<", ">", "==", "!=", "<=", ">=", "(", ")", ",")
  lexical.reserved += ("var", "println", "do", "times", "enddo", "if", "else", "endif", "function", "return", "endfunction", "main", "endmain")

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }

  //rep(a) = expect zero or more repetitions of a
  //"main" = reserved word from lexical.reserve
  //codeblock, function = parser rules defined further down
  //(a ~ b) = ensure that operand "a" is followed by operand "b"
  //(a ~> b) = ensure that operand "a" exists followed by operand "b", but don't include "a" in the result
  //<~ same as ~> but in reverse
  //^^ = if parsed successfully, transform the result using the block on the right
  def program: Parser[Program] = (rep(function) <~ "main" ) ~ codeblock <~ "endmain" ^^ {
    case a ~ b => {
      println("Found Main Program")
      new Program(a, b)
    }
  }

  //opt(b) = operand "b" is optional, ensure that if it exists the match is successfully
  def function: Parser[Function] = ("function" ~> ident) ~ ("(" ~> arguments  <~ ")") ~ codeblock ~ opt("return" ~> expression) <~ "endfunction" ^^ {
    case a ~ b ~ c ~ None => {
      println("Found Function declaration with no parameters")
      new Function(a, b, c, Number(0))
    }
    case a ~ b ~ c ~ d => {
      println("Found Function declaration with parameters")
      new Function(a, b, c, d.get)
    }
  }

  def functionCall: Parser[FunctionCall] = ident ~ ("(" ~> functionCallArguments <~ ")") ^^ {
    case a ~ b => {
      println("Found Function call")
      new FunctionCall(a, b)
    }
  }

  //repsep(a, b) = matches the pattern of repeating "a" operators followed by "b" operators
  def functionCallArguments: Parser[Map[String, Expression]] = repsep(functionArgument, ",") ^^ {
    _ toMap
  }

  def functionArgument: Parser[(String, Expression)] = ident ~ ("=" ~> expression) ^^ {
    case a ~ b => (a, b)
  }

  def codeblock: Parser[List[Statement]] = rep(statement) ^^ {
    case a => a
  }

  //positioned(a, b) = decorates a parser's result with the start position of the input it consumed
  //(a | b) = matches a or b
  def statement: Parser[Statement] = positioned(variableAssignment | loopBlock | ifBlock | functionCall | printLineStatement) ^^ {
    case  a => a
  }

  def arguments: Parser[Map[String, Int]] = repsep(ident, ",") ^^ {
    case argumentList => {
      (for (a <- argumentList) yield (a -> 0)) toMap
    }
  }

  def variableAssignment: Parser[VariableDefinition] = ("var" ~> ident <~ "=") ~ (functionCall | expression) ^^ {
    case a ~ b => {
      println("Found Variable assignment")
      new VariableDefinition(a, b)
    }
  }

  def loopBlock: Parser[LoopBlock] = ("do" ~> iterations <~ "times") ~ (codeblock <~ "enddo") ^^ {
    case a ~ b => {
      println("Found Loop block")
      new LoopBlock(a, b)
    }
  }

  def printLineStatement: Parser[PrintLineStatement] = "println" ~> positioned(expression) ^^ {
    case a => new PrintLineStatement(a)
  }

  def ifBlock: Parser[IfBlock] = ("if" ~> ("(" ~> condition <~ ")")) ~ codeblock ~ opt("else" ~> codeblock) <~ "endif" ^^ {
    case a ~ b ~ c => {
      c match {
        case None => {
          println("Found If Block With no Else Block")
          new IfBlock(a, b, List())
        }
        case _ => {
          println("Found If Block + Else Block")
          new IfBlock(a, b, c.get)
        }
      }
    }
  }

  def condition: Parser[Condition] = positioned(expression) ~ ("<" | ">" | "==" | "!=" | "<=" | ">=") ~ positioned(expression) ^^ {
    case a ~ b ~ c => {
      new Condition(a, b, c)
    }
  }

  //numericLit = the class of numeric literal tokens
  def iterations: Parser[Int] = numericLit ^^ {
    case a => a.toInt
  }

  def expression: Parser[Expression] = term ~ rep(("+" | "-") ~ term) ^^ {
    //if we only have one term, return the term
    case a ~ List() => a
    //if we have more than one term, append the whole thing together
    case a ~ b => {

      def append(root: Operator, parent: Operator): Operator = {
        parent.leftPart = root
        parent
      }

      var root: Operator = new Operator(a, b.head._1, b.head._2)

      for (i <- b.tail) {
        val parent = i._1 match {
          case "+" => new Operator(null, i._1, i._2)
          case "-" => new Operator(null, i._1, i._2)
        }
        root = append(root, parent)
      }

      root
    }
  }

  def term: Parser[Expression] = multiplydivide ^^ { a => a} | factor ^^ { a => a}

  def multiplydivide: Parser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {

    case a ~ List() => a
    case a ~ b => {

      //function for appending the whole expression together
      def appendExpression(op1: Operator, op2: Operator): Operator = {
        op2.leftPart = op1.rightPart
        op1.rightPart = op2
        op2
      }

      val root: Operator = new Operator(a, b.head._1, b.head._2)
      var current = root

      for (i <- b.tail) {
        val rightOperator =
          i._1 match {
            case "*" => Operator(null, i._1, i._2)
            case "/" => Operator(null, i._1, i._2)
          }

        current = appendExpression(current, rightOperator)
      }

      root
    }
  }

  def factor: Parser[Expression] = numericLit ^^ { a => Number(a.toInt) } | "(" ~> expression <~ ")" ^^ { a => a } | ident ^^ {
    new Identifier(_)
  }

}

