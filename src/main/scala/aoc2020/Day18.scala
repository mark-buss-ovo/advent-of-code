package aoc2020

object Day18 {

  sealed trait ExpressionItem

  case class Expression(items: List[ExpressionItem]) {
    def evaluate(): Int = {
      def loop(remainingItems: List[ExpressionItem], operator: Option[Operator], totalSoFar: Int) = {
        val nextItem = remainingItems.head

        val nextOperator = nextItem match {
          case Operator => Some()
        }

        if(remainingItems.tail.isEmpty) {
          newTotalSoFar
        } else {
          loop(remainingItems.tail, )
        }
      }
    }
  }

  sealed trait Operator extends ExpressionItem

  object Operator {
    case object Plus extends Operator
    case object Multiply extends Operator
  }

  sealed trait Operand extends ExpressionItem {
    def evaluate(): Int
  }

  object Operand {
    case class Group(expression: Expression) extends Operand {

    }
  }

}
