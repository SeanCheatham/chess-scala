package com.seancheatham

package object chess {

  //
  // Piece-type Constants
  //
  final val _I: Byte =
    0

  final val _E: Byte =
    1

  final val BP: Byte =
    2

  final val BB: Byte =
    3

  final val BN: Byte =
    4

  final val BR: Byte =
    5

  final val BQ: Byte =
    6

  final val BK: Byte =
    7

  final val WP: Byte =
    8

  final val WB: Byte =
    9

  final val WN: Byte =
    10

  final val WR: Byte =
    11

  final val WQ: Byte =
    12

  final val WK: Byte =
    13

  //
  // Piece-type Weight Constants
  //
  final val P_WEIGHT: Double =
    1

  final val B_WEIGHT: Double =
    3

  final val N_WEIGHT: Double =
    3

  final val R_WEIGHT: Double =
    5

  final val Q_WEIGHT: Double =
    9

  final val K_WEIGHT: Double =
    10000

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
  final val LEGAL_SQUARES: Vector[Byte] =
    ((20 until 29).toVector ++
      (30 until 39) ++
      (40 until 49) ++
      (50 until 59) ++
      (60 until 69) ++
      (70 until 79) ++
      (80 until 89) ++
      (90 until 99))
    .map(_.toByte)

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
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I  // 119
    )

  final val CHALLENGE_BOARD_SQUARES =
    Vector[Byte](
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 9
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 19
      _I, _E, BK, _E, BR, _E, BN, BR, _E, _I,
      _I, BP, _E, BQ, _E, BB, BP, _E, _E, _I,
      _I, BP, BB, BP, _E, BP, _E, _E, _E, _I,
      _I, _E, _E, WN, BP, WP, BN, _E, BP, _I,
      _I, WP, WP, _E, WP, _E, _E, BP, _E, _I,
      _I, _E, _E, WP, _E, _E, _E, _E, _E, _I,
      _I, _E, _E, _E, _E, WQ, WP, WP, WP, _I,
      _I, WR, _E, WB, _E, WN, WR, WK, _E, _I,
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I, // 109
      _I, _I, _I, _I, _I, _I, _I, _I, _I, _I  // 119

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
  }

}
