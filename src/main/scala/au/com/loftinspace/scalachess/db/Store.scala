package au.com.loftinspace.scalachess.db

import game.Game

object Store {
  var onlyGame = new Game
  def loadGame(id: Int): Game = onlyGame
  def saveGame(id: Int, game: Game) = { onlyGame = game }
}