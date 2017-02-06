package com.rpn

import scala.collection.immutable.Stack
import scala.util.Try

object RpnCalculator {
  def calculate(calculation: String): String =
    calculateSteps(parse(calculation)).map(_.number).mkString(" ")

  private def calculateSteps(steps: List[Step]): List[Number] =
    steps
      .foldLeft(Stack.empty[Number]) { (accumulate, step) =>
        step match {
          case number: Number => accumulate.push(number)
          case operand: Operand =>
            val (lastNumber, queueWithoutLastNumber) = accumulate.pop2
            val (penultimateNumber, queueWithoutPenultimateNumber) =
              queueWithoutLastNumber.pop2
            queueWithoutPenultimateNumber.push(
              operand.calculate(lastNumber, penultimateNumber))
        }
      }
      .toList
      .reverse

  private def parse(calculation: String): List[Step] =
    calculation.split(" ").toList.map { unparsedStep =>
      if (isNumber(unparsedStep)) {
        Number(unparsedStep.toInt)
      } else {
        Operand(unparsedStep)
      }
    }

  private def isNumber(unparsedStep: String): Boolean =
    Try(unparsedStep.toInt).isSuccess

  private def isOperand(step: Step): Boolean = step match {
    case Operand(_) => true
    case _ => false
  }
}

trait Step

case class Number(number: Int) extends Step

case class Operand(value: String) extends Step {
  def calculate(first: Number, second: Number): Number = value match {
    case "+" => Number(first.number + second.number)
    case "-" => Number(first.number - second.number)
    case "*" => Number(first.number * second.number)
    case "/" => Number(first.number / second.number)
  }
}
