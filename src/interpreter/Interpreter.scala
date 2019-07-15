package interpreter
import parser._

class Interpreter(program: Program) {

  def run() {
    iterate(program.listOfStatements)
  }

  var scope = new Scope("global", null)

  private def calculator(expression: Expression): Int = {
    expression match {
      case Number(value) => {
        value
      }
      case Identifier(name) => {
        calculator(getVariable(expression.asInstanceOf[Identifier]))
      }
      case Operator(leftPart, operator, rightPart) => {
        operator match {
          case "+" => calculator(leftPart) + calculator(rightPart)
          case "-" => calculator(leftPart) - calculator(rightPart)
          case "*" => calculator(leftPart) * calculator(rightPart)
          case "/" => calculator(leftPart) / calculator(rightPart)
        }
      }
    }
  }

  //Get the variable
  private def getVariable(identifier: Identifier): Expression = {
    var s: Scope = scope
    while ((!s.name.equals("global")) && !s.variables.contains(identifier.name)) {
      s = s.parentScope
    }
    s.variables(identifier.name)
  }

  //Determines the boolean value of a condition
  private def determineCondition(condition: Condition): Boolean = {
    val a = calculator(condition.leftPart)
    val b = calculator(condition.rightPart)

    condition.operator match {
      case "==" => (a == b)
      case "!=" => (a != b)
      case "<=" => (a <= b)
      case "<" => (a < b)
      case ">=" => (a >= b)
      case ">" => (a > b)
    }
  }

  //Iterate across the list of statements and run the equivalent scala code for each of them
  private def iterate(listOfStatements: List[Statement]) {
    if (!listOfStatements.isEmpty) {

      listOfStatements.head match {

        case FunctionCall(name, values) => {
          val function = program.listOfFunctions.filter(a => a.functionName == name)
          scope = new Scope(function(0).functionName, scope)
          for (v <- values) {
            scope.variables(v._1) = v._2
          }
          iterate(function(0).listOfStatements)
          scope = scope.parentScope
          iterate(listOfStatements.tail)
        }

        case VariableDefinition(name, value) => {
          if (value.isInstanceOf[FunctionCall]) {
            val functionCall = value.asInstanceOf[FunctionCall]
            val function = program.listOfFunctions.filter(x => x.functionName == functionCall.name)
            scope = new Scope(function(0).functionName, scope)
            for (v <- functionCall.values) {
              scope.variables(v._1) = v._2
            }
            iterate(function(0).listOfStatements)
            scope = scope.parentScope
            scope.variables(name) = function(0).returnValue
            println("VARIABLE ASSIGNMENT HAS SUCCESSFULLY BEEN INFERRED TO BE FUNCTION RETURN TYPE")
          } else {
            scope.variables(name) = value
            println("VARIABLE ASSIGNMENT HAS SUCCESSFULLY BEEN INFERRED TO BE INTEGER")
          }
          iterate(listOfStatements.tail)
        }

        case PrintLineStatement(value) => {
          println(calculator(value))
          iterate(listOfStatements.tail)
        }

        case LoopBlock(iterations, statements) => {
          scope = new Scope("", scope)
          for (i <- 0 until iterations) {
            iterate(statements)
          }
          scope = scope.parentScope
          iterate(listOfStatements.tail)
        }

        case IfBlock(condition, trueBranch, falseBranch) => {
          scope = new Scope("", scope)
          if (determineCondition(condition)) {
            iterate(trueBranch)
          } else {
            iterate(falseBranch)
          }
          scope = scope.parentScope
          iterate(listOfStatements.tail)
        }
      }
    }
  }
}