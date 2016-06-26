package com.seancheatham.chess

/**
  * The state representation of a chess board.
  *
  * A "Board" provides the functionality for search, evaluation, and move
  * generation (the core components of a chess engine).
  *
  * @param pieces A sequence of Bytes of length 120.  An actual chess board consists of an 8x8 grid; however,
  *               to allow for efficient move generation for knights, the board is padded by two squares on the top
  *               and bottom, and one square on the left and right side.  This board is represented with
  *               10 columns and 12 rows.  This improves move generation for knights by being able to detect
  *               an "out-of-bounds" square without having to deal with null pointer exceptions.  It also
  *               simplifies the math involved with move generation, since each row has ten slots,
  *               which correspond nicely to base-10 arithmetic
  * @param whiteToMove Flag indicating if it is White's turn to move.  False indicates it is Black's turn to move.
  * @param blackKingCastleAvailable Flag indicating if the Black King-side Castle is still available
  * @param blackQueenCastleAvailable Flag indicating if the Black Queen-side Castle is still available
  * @param whiteKingCastleAvailable Flag indicating if the White King-side Castle is still available
  * @param whiteQueenCastleAvailable Flag indicating if the White Queen-side Castle is still available
  * @param enPassant An optional index to the "en passant" square from a previous move
  * @param moveCount The number of moves which have occured on the board thus far
  */
case class Board(pieces: Vector[Byte],
                 whiteToMove: Boolean,
                 blackKingCastleAvailable: Boolean,
                 blackQueenCastleAvailable: Boolean,
                 whiteKingCastleAvailable: Boolean,
                 whiteQueenCastleAvailable: Boolean,
                 enPassant: Option[Byte],
                 moveCount: Short) {

  /**
    * Determines the available moves which can be made by the current side to move.  The results of
    * this list are assumed to be valid moves, with the exception of moves which would
    * put the current player into a "checked" position, or if the current player does not move
    * out of an already "checked" position.  In these cases, we simply let the next turn
    * take place, where the opposing player would capture the king, resulting in a "check mate"
    *
    * To represent a "castle", the "from" index is the index of the King, and the "to" index is the
    * position to which the king will be moved after the castle.
    * The movement of the Rook is assumed implicitly.
    *
    * @return a sequence of (from, to) tuples, indicating the index of the piece being moved,
    *         and the index to move to
    */
  def availableMoves: Vector[(Byte, Byte)] = {
    def diagonals(index: Int) = {
      var results =
        Vector.empty[Int]

      var i =
        index - 9

      var done =
        false

      def handle(i1: Int,
                 incrementer: Int) =
        if (pieces(i).isEmpty) {
          results = results :+ i
          i = i + incrementer
        }
        else if (if (whiteToMove) pieces(i).isBlack else pieces(i).isWhite) {
          results = results :+ i
          done = true
        }
        else
          done = true

      while (i >= 21 && !done)
        handle(i, -9)

      i = index - 11
      done = false

      while (i >= 21 && !done)
        handle(i, -11)

      i = index + 9
      done = false

      while (i <= 98 && !done)
        handle(i, 9)

      i = index + 11
      done = false

      while (i <= 98 && !done)
        handle(i, 11)


      results map ((index, _))
    }

    def horizontalsVerticals(index: Int) = {
      var results =
        Vector.empty[Int]

      var i =
        index - 10

      var done =
        false

      def handle(i1: Int,
                 incrementer: Int) =
        if (pieces(i).isEmpty) {
          results = results :+ i
          i = i + incrementer
        }
        else if (if (whiteToMove) pieces(i).isBlack else pieces(i).isWhite) {
          results = results :+ i
          done = true
        }
        else
          done = true

      while (i >= 21 && !done)
        handle(i, -10)

      i = index - 1
      done = false

      while (i >= 21 && !done)
        handle(i, -1)

      i = index + 10
      done = false

      while (i <= 98 && !done)
        handle(i, 10)

      i = index + 1
      done = false

      while (i <= 98 && !done)
        handle(i, 1)


      results map ((index, _))
    }

    def pawn(index: Int) = {
      val direction =
        if (whiteToMove) -1 else 1
      val downLeft =
        if (
          if (whiteToMove) pieces(index + 9 * direction).isBlack
          else pieces(index + 9 * direction).isWhite
        )
          Some((index, index + 9 * direction))
        else
          None
      val downRight =
        if (
          if (whiteToMove) pieces(index + 11 * direction).isBlack
          else pieces(index + 11 * direction).isWhite
        )
          Some((index, index + 11 * direction))
        else
          None
      val downOne =
        if (
          if (whiteToMove) pieces(index + 10 * direction).isBlack
          else pieces(index + 10 * direction).isWhite
        )
          Some((index, index + 10 * direction))
        else
          None
      val downTwo =
        if (index / 10 == (if (whiteToMove) 8 else 3) &&
          pieces(index + 10 * direction).isEmpty &&
          pieces(index + 20 * direction).isEmpty
        )
          Some((index, index + 20 * direction))
        else
          None
      Vector(downLeft, downRight, downOne, downTwo).flatten
    }

    def bishop =
      diagonals _

    def knight(index: Int) =
      Vector(-21, -19, -12, -8, 8, 12, 19, 21)
        .flatMap { offset =>
          val s =
            pieces(index + offset)
          if (if (whiteToMove) s.isBlack else s.isWhite || s.isEmpty)
            Some(index + offset)
          else
            None
        }
        .map((index, _))

    def rook =
      horizontalsVerticals _

    def queen(index: Int) =
      diagonals(index) ++
        horizontalsVerticals(index)

    def king(index: Int) =
      Vector(-11, -10, -9, -1, 1, 9, 10, 11)
        .flatMap { offset =>
          val s =
            pieces(index + offset)
          if (if (whiteToMove) s.isBlack else s.isWhite || s.isEmpty)
            Some(index + offset)
          else
            None
        }
        .map((index, _))

    val bkCastle =
      if (!whiteToMove &&
        blackKingCastleAvailable &&
        pieces(26).isEmpty &&
        pieces(27).isEmpty
      )
        Some((25.toByte, 27.toByte))
      else
        None

    val bqCastle =
      if (!whiteToMove &&
        blackQueenCastleAvailable &&
        pieces(22).isEmpty &&
        pieces(23).isEmpty &&
        pieces(24).isEmpty
      )
        Some((25.toByte, 23.toByte))
      else
        None

    val wkCastle =
      if (whiteToMove &&
        whiteKingCastleAvailable &&
        pieces(96).isEmpty &&
        pieces(97).isEmpty
      )
        Some((95.toByte, 97.toByte))
      else
        None

    val wqCastle =
      if (whiteToMove &&
        whiteQueenCastleAvailable &&
        pieces(92).isEmpty &&
        pieces(93).isEmpty &&
        pieces(94).isEmpty
      )
        Some((95.toByte, 93.toByte))
      else
        None

    Vector(bkCastle, bqCastle, wkCastle, wqCastle).flatten ++
      pieces
        .indices
        .toVector
        .filter(index => if (whiteToMove) pieces(index).isWhite else pieces(index).isBlack)
        .flatMap(index =>
          pieces(index) match {
            case `_I` | `_E` =>
              Vector.empty
            case `BP` | `WP` =>
              pawn(index)
            case `BB` | `WB` =>
              bishop(index)
            case `BN` | `WN` =>
              knight(index)
            case `BR` | `WR` =>
              rook(index)
            case `BQ` | `WQ` =>
              queen(index)
            case `BK` | `WK` =>
              king(index)
          }
        )
        .map { case (f, t) => (f.toByte, t.toByte) }
        // To assist with the "pruning" in alpha/beta pruning, sort the result
        // by the "weight" of the piece at the destination index.  Pieces
        // with higher weights will be searched first.
        .sortBy(move => -pieces(move._2).weight)
  }

  /**
    * Executes the move from the given index to the given index.  The value at the "from" index is set to Empty.
    * Any piece at the "to" index will be captured/removed from the board.
    *
    * @param from The index of the position from which the piece is moving
    * @param to   The target index to move to
    * @return A new instance of the updated Board, with all of the proper flags set
    */
  def move(from: Byte,
           to: Byte): Board = {

    val isBKCastle: Boolean =
      blackKingCastleAvailable &&
        (
          pieces(from.toInt) == BK &&
            from == 25 &&
            to == 27
          )

    val isBQCastle: Boolean =
      blackQueenCastleAvailable &&
        !isBKCastle &&
        (
          pieces(from.toInt) == BK &&
            from == 25 &&
            to == 23
          )

    val isWKCastle: Boolean =
      whiteKingCastleAvailable &&
        !isBKCastle &&
        !isBQCastle &&
        (
          pieces(from.toInt) == WK &&
            from == 95 &&
            to == 97
          )

    val isWQCastle: Boolean =
      whiteQueenCastleAvailable &&
        !isBKCastle &&
        !isBQCastle &&
        !isWKCastle &&
        (
          pieces(from.toInt) == WK &&
            from == 95 &&
            to == 93
          )

    val newEnPassant: Option[Byte] =
      None // TODO: Implement

    Board(
      pieces
        .updated(to.toInt, pieces(from.toInt))
        .updated(from.toInt, 1.toByte),
      !whiteToMove,
      (blackKingCastleAvailable || isBKCastle) && from != 28 && from != 25,
      (blackQueenCastleAvailable || isBQCastle) && from != 21 && from != 25,
      (whiteKingCastleAvailable || isWKCastle) && from != 98 && from != 95,
      (whiteQueenCastleAvailable || isWQCastle) && from != 91 && from != 95,
      newEnPassant,
      (moveCount + 1).toByte
    )
  }

  /**
    * Determines if there is a winner of the match, by checking for the non-existence of a King for each side.
    *
    * @return An option indicating the "side" which won.
    *         Some([[com.seancheatham.chess#BLACK]]) if Black won the match
    *         Some([[com.seancheatham.chess#WHITE]]) if White won the match
    *         None if the game is still in progress
    */
  def winner: Option[Byte] =
    if (pieces contains BK)
      if (pieces contains WK)
        None
      else
        Some(BLACK)
    else
      Some(WHITE)

  /**
    * Evaluates the board in its current state.
    *
    * @return A Double value representing the "favorability" of a
    *         particular side.  A negative value indicates that the BLACK side is favored; a positive value indicates that
    *         the WHITE side is favored; a value of zero represents a balanced game
    */
  def evaluate: Double = {

    // The final evaluation result
    var result: Double =
      0

    val pawnCount =
      pieces count (piece => piece == BP || piece == WP)

    val bishopWeightFactor: Double =
      1 + (16d - pawnCount) / 64

    val knightWeightFactor: Double =
      1 - (16d - pawnCount) / 64

    result =
      result +
        LEGAL_SQUARES
          .map(index =>
            pieces(index) match {
              case `_I` | `_E` =>
                pieces(index).weight
              case `BP` =>
                -pieces(index).weight *
                  (1 -
                    (Range(index % 10, 98, 10)
                      .count(i => pieces(i) == BP) - 1) / 4
                    )
              case `BB` =>
                -pieces(index).weight * bishopWeightFactor
              case `BN` =>
                -pieces(index).weight * knightWeightFactor
              case `BR` | `BQ` | `BK` =>
                -pieces(index).weight
              case `WP` =>
                pieces(index).weight *
                  (1 -
                    (Range(index % 10, 98, 10)
                      .count(i => pieces(i) == WP) - 1) / 4
                    )
              case `WB` =>
                pieces(index).weight * bishopWeightFactor
              case `WN` =>
                pieces(index).weight * knightWeightFactor
              case `WR` | `WQ` | `WK` =>
                pieces(index).weight
            }
          )
          .sum

    result
  }

  /**
    * Performs alpha/beta search (with pruning) on the current board
    *
    * @param depth The maximum depth to search
    * @return The best move to be made from the available moves
    */
  def search(depth: Int): (Byte, Byte) =
    availableMoves
      .par
      .map(m =>
        m -> move(m._1, m._2).alphaBetaMax(Double.MinValue, Double.MaxValue, depth)
      )
      .maxBy(_._2)
      ._1

  /**
    * Computes the "max" side of alpha/beta search, by attempting to maximize the value returned
    *
    * @param alpha          The "alpha" value
    * @param beta           The "beta" value
    * @param remainingDepth The number of levels remaining to be searched
    * @return A Double indicating the "max" evaluated result of child boards
    */
  def alphaBetaMax(alpha: Double, beta: Double, remainingDepth: Int): Double =
    if (remainingDepth <= 0)
      evaluate
    else {
      var v = Double.MinValue
      var a = alpha
      val _availableMoves = availableMoves.iterator
      var continue = true
      while (continue && _availableMoves.hasNext) {
        val (from, to) =
          _availableMoves.next()
        v = Math.max(v, move(from, to).alphaBetaMin(a, beta, remainingDepth - 1))
        a = Math.max(a, v)
        if (beta <= a)
          continue = false
      }
      v
    }

  /**
    * Computes the "min" side of alpha/beta search, by attempting to minimize the value returned
    *
    * @param alpha          The "alpha" value
    * @param beta           The "beta" value
    * @param remainingDepth The number of levels remaining to be searched
    * @return A Double indicating the "min" evaluated result of child boards
    */
  def alphaBetaMin(alpha: Double, beta: Double, remainingDepth: Int): Double =
    if (remainingDepth <= 0)
      -evaluate
    else {
      var v = Double.MaxValue
      var b = beta
      val _availableMoves = availableMoves.iterator
      var continue = true
      while (continue && _availableMoves.hasNext) {
        val (from, to) =
          _availableMoves.next()
        v = Math.min(v, move(from, to).alphaBetaMax(alpha, b, remainingDepth - 1))
        b = Math.min(b, v)
        if (b <= alpha)
          continue = false
      }
      v
    }
}

object Board {

  final val default: Board =
    Board(
      DEFAULT_BOARD_SQUARES,
      whiteToMove = true,
      blackKingCastleAvailable = true,
      blackQueenCastleAvailable = true,
      whiteKingCastleAvailable = true,
      whiteQueenCastleAvailable = true,
      enPassant = None,
      moveCount = 0
    )

  final val challenge: Board =
    Board(
      CHALLENGE_BOARD_SQUARES,
      whiteToMove = true,
      blackKingCastleAvailable = false,
      blackQueenCastleAvailable = false,
      whiteKingCastleAvailable = false,
      whiteQueenCastleAvailable = false,
      enPassant = None,
      moveCount = 14
    )

}
