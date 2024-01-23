package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day22 {

  private val filenamePart1Sample = "2020/Day22/part1-sample.txt"
  private val filenamePart1Input = "2020/Day22/part1-input.txt"

  private type Card = Int
  private case class Deck(cards: List[Card]) {
    def draw(): (Card, Deck) = {
      (cards.head, Deck(cards.tail))
    }

    def add(cardsToAdd: List[Card]): Deck = {
      Deck(cards.appendedAll(cardsToAdd))
    }

    def nextCards(num: Int): Deck = {
      Deck(cards.take(num))
    }
  }
  private case class Game(player1Deck: Deck, player2Deck: Deck) {
    def isOver(): Boolean =
      player1Deck.cards.isEmpty || player2Deck.cards.isEmpty

    def playRound(): Game = {
      val (p1Card, p1UpdatedDeck) = player1Deck.draw()
      val (p2Card, p2UpdatedDeck) = player2Deck.draw()

      if (p1Card > p2Card) {
        Game(p1UpdatedDeck.add(List(p1Card, p2Card)), p2UpdatedDeck)
      } else {
        Game(p1UpdatedDeck, p2UpdatedDeck.add(List(p2Card, p1Card)))
      }
    }

    def winningScore(): Int = {
      val winningDeck =
        if (player1Deck.cards.nonEmpty) player1Deck else player2Deck

      winningDeck.cards.reverse.zipWithIndex.map { case (card, index) =>
        card * (index + 1)
      }.sum
    }
  }

  private def parseDecks(filename: String): Game = {
    val contents = Source.fromResource(filename).getLines().mkString("\n")

    def parseDeck(deckString: String): Deck = {
      Deck(deckString.split("\n").tail.map[Card](_.toInt).toList)
    }

    contents.split("\n\n") match {
      case Array(player1String, player2String) =>
        Game(parseDeck(player1String), parseDeck(player2String))
    }
  }

  private def playGame(filename: String): Int = {

    val initialGame = parseDecks(filename)
    @tailrec
    def doRound(gameInProgress: Game): Int = {
      val nextRound = gameInProgress.playRound()
      if (nextRound.isOver()) {
        nextRound.winningScore()
      } else {
        doRound(nextRound)
      }
    }

    doRound(initialGame)
  }

  private case class GameState(player1Deck: Deck, player2Deck: Deck)

  private case class RecursiveGame(
      currentState: GameState,
      previousStates: List[GameState],
      subGameLevel: Int
  ) {
    def isOver(): Boolean = {
      previousStates.contains(
        currentState
      ) || currentState.player1Deck.cards.isEmpty || currentState.player2Deck.cards.isEmpty
    }

    def playToEnd(): (RecursiveGame, Boolean) = {

      @tailrec
      def doAllRounds(
          game: RecursiveGame,
          roundNumber: Int
      ): (RecursiveGame, Boolean) = {
        println(
          s"${if (subGameLevel > 0) s"Sub-game level $subGameLevel - "}Round $roundNumber"
        )
        val nextRound = game.playRound()
        if (nextRound.isOver()) {
          val p1Won = previousStates.contains(
            nextRound.currentState
          ) || nextRound.currentState.player2Deck.cards.isEmpty

          (nextRound, p1Won)
        } else {
          doAllRounds(nextRound, roundNumber + 1)
        }
      }

      doAllRounds(this, 1)
    }

    def playRound(): RecursiveGame = {
      val (p1Card, p1UpdatedDeck) = currentState.player1Deck.draw()
      val (p2Card, p2UpdatedDeck) = currentState.player2Deck.draw()

      val nextState = if (
        p1UpdatedDeck.cards.size >= p1Card && p2UpdatedDeck.cards.size >= p2Card
      ) {
        val subGame =
          RecursiveGame(
            GameState(
              p1UpdatedDeck.nextCards(p1Card),
              p2UpdatedDeck.nextCards(p2Card)
            ),
            List.empty,
            subGameLevel + 1
          )
        val (_, p1Won) = subGame.playToEnd()
        if (p1Won) {
          GameState(p1UpdatedDeck.add(List(p1Card, p2Card)), p2UpdatedDeck)
        } else {
          GameState(p1UpdatedDeck, p2UpdatedDeck.add(List(p2Card, p1Card)))
        }
      } else {
        if (p1Card > p2Card) {
          GameState(p1UpdatedDeck.add(List(p1Card, p2Card)), p2UpdatedDeck)
        } else {
          GameState(p1UpdatedDeck, p2UpdatedDeck.add(List(p2Card, p1Card)))
        }
      }
      RecursiveGame(
        nextState,
        previousStates.appended(currentState),
        subGameLevel
      )
    }

    def winningScore(): Int = {
      val winningDeck =
        if (
          previousStates.contains(
            currentState
          ) || currentState.player1Deck.cards.nonEmpty
        ) currentState.player1Deck
        else currentState.player2Deck

      winningDeck.cards.reverse.zipWithIndex.map { case (card, index) =>
        card * (index + 1)
      }.sum
    }
  }

  private def playRecursiveGame(filename: String): Int = {
    val game = parseDecks(filename)
    val initialState = GameState(game.player1Deck, game.player2Deck)
    val recursiveGame = RecursiveGame(initialState, List.empty, 0)

    val (endGame, _) = recursiveGame.playToEnd()
    endGame.winningScore()
  }

  def main(args: Array[String]): Unit = {
    println(s"Part 1 Sample: ${playGame(filenamePart1Sample)}")
    println(s"Part 1 Input: ${playGame(filenamePart1Input)}")
    println(s"Part 2 Sample: ${playRecursiveGame(filenamePart1Sample)}")
    println(s"Part 2 Input: ${playRecursiveGame(filenamePart1Input)}")
  }

}
