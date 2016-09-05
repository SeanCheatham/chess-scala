package com.seancheatham.chess

object Evaluate {

  /**
    * Evaluates the board in its current state.
    *
    * @param board The board to evaluate
    * @return A Double value representing the "favorability" of a
    *         particular side.  A negative value indicates that the BLACK side is favored; a positive value indicates that
    *         the WHITE side is favored; a value of zero represents a balanced game
    */
  def apply(board: Board): Double = {

    val pawnCount =
      board.pieces count (piece => piece == BP || piece == WP)

    val bishopWeightFactor: Double =
      1 + (16d - pawnCount) / 64

    val knightWeightFactor: Double =
      1 - (16d - pawnCount) / 64

    LEGAL_SQUARES
      .map(index =>
        board.pieces(index) match {
          case `_I` | `_E` =>
            board.pieces(index).weight
          case `BP` =>
            -board.pieces(index).weight *
              (1 -
                (Range(index % 10, 98, 10)
                  .count(i => board.pieces(i) == BP) - 1) / 4
                )
          case `BB` =>
            -board.pieces(index).weight * bishopWeightFactor
          case `BN` =>
            -board.pieces(index).weight * knightWeightFactor
          case `BR` | `BQ` | `BK` =>
            -board.pieces(index).weight
          case `WP` =>
            board.pieces(index).weight *
              (1 -
                (Range(index % 10, 98, 10)
                  .count(i => board.pieces(i) == WP) - 1) / 4
                )
          case `WB` =>
            board.pieces(index).weight * bishopWeightFactor
          case `WN` =>
            board.pieces(index).weight * knightWeightFactor
          case `WR` | `WQ` | `WK` =>
            board.pieces(index).weight
        }
      )
      .sum
  }

}
