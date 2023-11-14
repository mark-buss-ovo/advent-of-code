package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day21 {
  private val filenamePart1Sample = "2020/Day21/part1-sample.txt"
  private val filenamePart1Input = "2020/Day21/part1-input.txt"

  private case class Food(ingredients: List[String], allergens: List[String])

  private def getFoods(filename: String): List[Food] = {
    val lines = Source.fromResource(filename).getLines()

    lines
      .map(l =>
        l.split('(') match {
          case Array(ingredients, allergens) =>
            Food(
              ingredients.split(' ').toList,
              allergens
                .substring(
                  "contains ".length,
                  allergens.length - 1
                )
                .split(", ")
                .toList
            )
        }
      )
      .toList
  }

  private def getAllergenIngredientMap(
      foods: List[Food]
  ): Map[String, Set[String]] = {
    val allAllergens = foods.flatMap(_.allergens).toSet

    allAllergens
      .map(allergen => {
        val foodsContainingAllergen =
          foods.filter(_.allergens.contains(allergen))
        val allIngredients =
          foodsContainingAllergen.flatMap(_.ingredients).toSet
        val ingredientsCouldContainAllergen =
          allIngredients.filter(i =>
            foodsContainingAllergen.forall(f => f.ingredients.contains(i))
          )
        allergen -> ingredientsCouldContainAllergen
      })
      .toMap
  }

  private def countNonAllergenFoods(filename: String): Int = {
    val foods = getFoods(filename)

    val allergenToIngredientMap = getAllergenIngredientMap(foods)

    val allIngredients = foods.flatMap(_.ingredients).toSet
    val ingredientsPotentiallyContainingAllergens =
      allergenToIngredientMap.values.flatten.toSet
    val ingredientsWithoutAllergens =
      allIngredients.diff(ingredientsPotentiallyContainingAllergens)

    ingredientsWithoutAllergens.toList
      .map(i => foods.count(f => f.ingredients.contains(i)))
      .sum
  }

  private def getCanonicalDangerousIngredientList(filename: String): String = {
    val foods = getFoods(filename)
    val allergenToPotentialIngredientMap = getAllergenIngredientMap(foods)

    val (alreadyAssigned, stillToAssign) =
      allergenToPotentialIngredientMap.partition { case (_, ingredients) =>
        ingredients.size == 1
      }

    val initialMap = alreadyAssigned.view.mapValues(_.head).toMap

    val initialRemaining = stillToAssign.view
      .mapValues(_.filterNot(initialMap.values.toList.contains(_)))
      .toMap

    @tailrec
    def buildMap(
        assigned: Map[String, String],
        remainingAllergens: Map[String, Set[String]]
    ): Map[String, String] = {
      val (allergen, potentialIngredients) = remainingAllergens.toList.minBy {
        case (_, ingredients) => ingredients.size
      }

      val unassignedIngredients =
        potentialIngredients.filterNot(assigned.values.toList.contains(_))
      if (unassignedIngredients.size != 1) {
        throw new Exception(
          s"Couldn't choose ingredient for $allergen: $unassignedIngredients"
        )
      }

      val newAssigned = assigned.updated(allergen, unassignedIngredients.head)
      val newRemaining = remainingAllergens
        .removed(allergen)
        .view
        .mapValues(_.filterNot(newAssigned.values.toList.contains(_)))
        .toMap

      if (newRemaining.isEmpty) {
        newAssigned
      } else {
        buildMap(newAssigned, newRemaining)
      }
    }

    val assigned = buildMap(initialMap, initialRemaining)

    assigned.keys.toList.sorted.map(assigned(_)).mkString(",")
  }

  def main(args: Array[String]): Unit = {
    println(s"Part 1 Sample: ${countNonAllergenFoods(filenamePart1Sample)}")
    println(s"Part 1 Input: ${countNonAllergenFoods(filenamePart1Input)}")
    println(
      s"Part 2 Sample: ${getCanonicalDangerousIngredientList(filenamePart1Sample)}"
    )
    println(
      s"Part 2 Input: ${getCanonicalDangerousIngredientList(filenamePart1Input)}"
    )
  }
}
