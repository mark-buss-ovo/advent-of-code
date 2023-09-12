package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day16 {

  private val filenamePart1Sample = "2020/Day16/part1-sample.txt"
  private val filenamePart1Input = "2020/Day16/part1-input.txt"

  private case class FieldSpec(
      name: String,
      firstRange: Range,
      secondRange: Range
  ) {
    def containsAll(values: List[Long]): Boolean = {
      values.forall(v => firstRange.contains(v) || secondRange.contains(v))
    }
  }

  private case class Ticket(fields: List[Long]) {
    def invalidFields(fieldSpecs: List[FieldSpec]): List[Long] = {
      fields.filter(f =>
        !fieldSpecs.exists(s =>
          s.firstRange.contains(f) || s.secondRange.contains(f)
        )
      )
    }

    def isInvalid(fieldSpecs: List[FieldSpec]): Boolean = {
      fields.exists(f =>
        !fieldSpecs.exists(s =>
          s.firstRange.contains(f) || s.secondRange.contains(f)
        )
      )
    }
  }

  private case class Domain(
      fieldSpecs: List[FieldSpec],
      myTicket: Ticket,
      nearbyTickets: List[Ticket]
  ) {
    def discardInvalidTickets: Domain = {
      copy(nearbyTickets = nearbyTickets.filterNot(_.isInvalid(fieldSpecs)))
    }
  }

  private val fieldSpecRegex = """^([a-z\s]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  private def parseDomain(filename: String): Domain = {
    val contents = Source.fromResource(filename).mkString
    val sections = contents.split("\n\n")

    val fieldSpecs = sections(0)
      .split("\n")
      .map {
        case fieldSpecRegex(
              name,
              firstRangeMin,
              firstRangeMax,
              secondRangeMin,
              secondRangeMax
            ) =>
          FieldSpec(
            name,
            Range.inclusive(firstRangeMin.toInt, firstRangeMax.toInt),
            Range.inclusive(secondRangeMin.toInt, secondRangeMax.toInt)
          )
      }
      .toList

    val myTicket = Ticket(
      sections(1).split("\n").last.split(",").map(_.toLong).toList
    )

    val nearbyTickets = sections(2)
      .split("\n")
      .tail
      .map(_.split(",").map(_.toLong).toList)
      .map(Ticket)
      .toList

    Domain(fieldSpecs, myTicket, nearbyTickets)
  }

  private def calculateTicketScanningErrorRate(filename: String): Long = {
    val domain = parseDomain(filename)
    domain.nearbyTickets.flatMap(_.invalidFields(domain.fieldSpecs)).sum
  }

  private def calculateDepartureFieldsProduct(filename: String): Long = {
    val domain = parseDomain(filename).discardInvalidTickets

    val fieldValues: Map[Int, List[Long]] = domain.myTicket.fields.indices
      .map(i =>
        i ->
          List(domain.myTicket.fields(i))
            .appendedAll(domain.nearbyTickets.map(_.fields(i)))
      )
      .toMap

    val initialPossibleFields: Map[Int, List[FieldSpec]] = fieldValues.map {
      case (index, values) =>
        index -> domain.fieldSpecs.filter(_.containsAll(values))
    }

    @tailrec
    def assignFields(
        assignedFields: Map[FieldSpec, Int],
        possibleFields: Map[Int, List[FieldSpec]]
    ): Map[FieldSpec, Int] = {

      val smallestSetOfCandidates = possibleFields.toList
        .sortBy { case (_, candidates) =>
          candidates.size
        }
        .reverse
        .head

      smallestSetOfCandidates match {
        case (index, possibleFieldsForIndex) =>
          val otherIndices = possibleFields.filterNot { case (otherIndex, _) =>
            otherIndex == index
          }

          val mustBeField = possibleFieldsForIndex.filterNot(candidateField =>
            otherIndices.exists { case (_, otherCandidates) =>
              otherCandidates.contains(candidateField)
            }
          )

          if (mustBeField.size != 1) {
            throw new Exception(
              s"ERROR can't determine field for $index, ${mustBeField.size} candidates"
            )
          } else {
            val updatedFields = assignedFields.updated(mustBeField.head, index)
            val updatedPossibleFields =
              possibleFields.removed(index).map { case (i, updatedFields) =>
                i -> updatedFields.filter(f => f != mustBeField.head)
              }

            if (updatedFields.size == domain.fieldSpecs.size) {
              updatedFields
            } else {
              assignFields(updatedFields, updatedPossibleFields)
            }
          }
      }
    }

    val assignedFields =
      assignFields(Map.empty[FieldSpec, Int], initialPossibleFields)

    assignedFields
      .collect {
        case (field, index) if field.name.startsWith("departure") => index
      }
      .map(index => domain.myTicket.fields(index))
      .product
  }

  def main(args: Array[String]): Unit = {
    println(calculateTicketScanningErrorRate(filenamePart1Sample))
    println(calculateTicketScanningErrorRate(filenamePart1Input))
    println(calculateDepartureFieldsProduct(filenamePart1Sample))
    println(calculateDepartureFieldsProduct(filenamePart1Input))

  }

}
