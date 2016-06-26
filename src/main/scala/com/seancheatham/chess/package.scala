package com.seancheatham

package object chess {

  //
  // Piece-type Constants
  //
  /**
    * An "invalid" square, or "out-of-bounds"
    */
  final val _I: Byte =
    0

  /**
    * An "empty" square
    */
  final val _E: Byte =
    1

  /**
    * Black Pawn
    */
  final val BP: Byte =
    2

  /**
    * Black Bishop
    */
  final val BB: Byte =
    3

  /**
    * Black Knight ("N" is a convention to avoid conflicting with the King)
    */
  final val BN: Byte =
    4

  /**
    * Black Rook
    */
  final val BR: Byte =
    5

  /**
    * Black Queen
    */
  final val BQ: Byte =
    6

  /**
    * Black King
    */
  final val BK: Byte =
    7

  /**
    * White Pawn
    */
  final val WP: Byte =
    8

  /**
    * White Bishop
    */
  final val WB: Byte =
    9

  /**
    * White Knight ("N" is a convention to avoid conflicting with the King)
    */
  final val WN: Byte =
    10

  /**
    * White Rook
    */
  final val WR: Byte =
    11

  /**
    * White Queen
    */
  final val WQ: Byte =
    12

  /**
    * White King
    */
  final val WK: Byte =
    13

  //
  // Team/Side Constants
  //
  final val BLACK: Byte =
    0

  final val WHITE: Byte =
    1

  //
  // Board Constants
  //
  /**
    * A collection of squares which are legal on the board.  Since a [[com.seancheatham.chess.Board]] is
    * represented as 10x12, the outer squares provide padding, but are illegal for play
    */
  final val LEGAL_SQUARES: Vector[Byte] =
    (
      (20 until 29).toVector ++
        (30 until 39) ++
        (40 until 49) ++
        (50 until 59) ++
        (60 until 69) ++
        (70 until 79) ++
        (80 until 89) ++
        (90 until 99)
      )
      .map(_.toByte)

  /**
    * The basic chess board representation
    */
  final val DEFAULT_BOARD_SQUARES =
    Vector[Byte](
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 9
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 19
      _I, BR, BN, BB, BQ, BK, BB, BN, BR, _I, // 29
      _I, BP, BP, BP, BP, BP, BP, BP, BP, _I, // 39
      _I, _E, _E, _E, _E, _E, _E, _E, _E, _I, // 49
      _I, _E, _E, _E, _E, _E, _E, _E, _E, _I, // 59
      _I, _E, _E, _E, _E, _E, _E, _E, _E, _I, // 69
      _I, _E, _E, _E, _E, _E, _E, _E, _E, _I, // 79
      _I, WP, WP, WP, WP, WP, WP, WP, WP, _I, // 89
      _I, WR, WN, WB, WQ, WK, WB, WN, WR, _I, // 99
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 109
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I // 119
    )

  /**
    * A "challenge" test board, for testing purposes
    */
  final val CHALLENGE_BOARD_SQUARES =
    Vector[Byte](
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 9
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 19
      _I, _E, BK, _E, BR, _E, BN, BR, _E, _I, // 29
      _I, BP, _E, BQ, _E, BB, BP, _E, _E, _I, // 39
      _I, BP, BB, BP, _E, BP, _E, _E, _E, _I, // 49
      _I, _E, _E, WN, BP, WP, BN, _E, BP, _I, // 59
      _I, WP, WP, _E, WP, _E, _E, BP, _E, _I, // 69
      _I, _E, _E, WP, _E, _E, _E, _E, _E, _I, // 79
      _I, _E, _E, _E, _E, WQ, WP, WP, WP, _I, // 89
      _I, WR, _E, WB, _E, WN, WR, WK, _E, _I, // 99
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 109
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I // 119

    )

  //
  // Implicit Helpers
  //
  implicit class PieceTypeImplicits(pieceType: Byte) {

    def isWhite: Boolean =
      Range(WP.toInt, WK.toInt) contains pieceType.toInt

    def isBlack: Boolean =
      Range(BP.toInt, BK.toInt) contains pieceType.toInt

    def isEmpty: Boolean =
      pieceType == _E

    def isIllegal: Boolean =
      pieceType == _I

    def weight: Double =
      pieceType match {
        case `_E` | `_I` => 0
        case `BP` | `WP` => 1
        case `BB` | `WB` => 3
        case `BN` | `WN` => 3
        case `BR` | `WR` => 5
        case `BQ` | `WQ` => 9
        case `BK` | `WK` => Double.MaxValue
      }
  }

}
