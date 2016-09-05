package com.seancheatham.chess

object Search {

  /**
    * Performs alpha/beta search (with pruning) on the given board
    *
    * @param depth The maximum depth to search
    * @param board The board to search
    * @return The best move to be made from the available moves
    */
  def apply(depth: Int = Config.SEARCH_DEPTH)(board: Board): (Byte, Byte) =
    board.availableMoves.par
      .maxBy(m =>
        alphaBetaMax(Double.MinValue, Double.MaxValue, depth)(
          board.move(m._1, m._2)
        )
      )

  /**
    * Computes the "max" side of alpha/beta search, by attempting to maximize the value returned
    *
    * @param alpha          The "alpha" value
    * @param beta           The "beta" value
    * @param remainingDepth The number of levels remaining to be searched
    * @param board The board to search
    * @return A Double indicating the "max" evaluated result of child boards
    */
  def alphaBetaMax(alpha: Double, beta: Double, remainingDepth: Int)(board: Board): Double =
    if (remainingDepth <= 0)
      board.evaluate
    else {
      var v = Double.MinValue
      var a = alpha
      val _availableMoves = board.availableMoves.iterator
      var continue = true
      while (continue && _availableMoves.hasNext) {
        val (from, to) =
          _availableMoves.next()
        v = Math.max(
          v,
          alphaBetaMin(a, beta, remainingDepth - 1)(board
            .move(from, to)
          )
        )
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
    * @param board The board to search
    * @return A Double indicating the "min" evaluated result of child boards
    */
  def alphaBetaMin(alpha: Double, beta: Double, remainingDepth: Int)(board: Board): Double =
    if (remainingDepth <= 0)
      -board.evaluate
    else {
      var v = Double.MaxValue
      var b = beta
      val _availableMoves = board.availableMoves.iterator
      var continue = true
      while (continue && _availableMoves.hasNext) {
        val (from, to) =
          _availableMoves.next()
        v = Math.min(v, alphaBetaMax(alpha, b, remainingDepth - 1)(board.move(from, to)))
        b = Math.min(b, v)
        if (b <= alpha)
          continue = false
      }
      v
    }

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
    * @param board The board to search
    * @return a sequence of (from, to) tuples, indicating the index of the piece being moved,
    *         and the index to move to
    */
  def availableMoves(board: Board): Vector[(Byte, Byte)] = {
    def diagonals(index: Int) = {
      var results =
        Vector.empty[Int]

      var i =
        index - 9

      var done =
        false

      def handle(i1: Int,
                 incrementer: Int) =
        if (board.pieces(i).isEmpty) {
          results = results :+ i
          i = i + incrementer
        }
        else if (if (board.whiteToMove) board.pieces(i).isBlack else board.pieces(i).isWhite) {
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
        if (board.pieces(i).isEmpty) {
          results = results :+ i
          i = i + incrementer
        }
        else if (if (board.whiteToMove) board.pieces(i).isBlack else board.pieces(i).isWhite) {
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
        if (board.whiteToMove) -1 else 1
      val downLeft =
        if (
          if (board.whiteToMove) board.pieces(index + 9 * direction).isBlack
          else board.pieces(index + 9 * direction).isWhite
        )
          Some((index, index + 9 * direction))
        else
          None
      val downRight =
        if (
          if (board.whiteToMove) board.pieces(index + 11 * direction).isBlack
          else board.pieces(index + 11 * direction).isWhite
        )
          Some((index, index + 11 * direction))
        else
          None
      val downOne =
        if (
          if (board.whiteToMove) board.pieces(index + 10 * direction).isBlack
          else board.pieces(index + 10 * direction).isWhite
        )
          Some((index, index + 10 * direction))
        else
          None
      val downTwo =
        if (index / 10 == (if (board.whiteToMove) 8 else 3) &&
          board.pieces(index + 10 * direction).isEmpty &&
          board.pieces(index + 20 * direction).isEmpty
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
            board.pieces(index + offset)
          if (if (board.whiteToMove) s.isBlack else s.isWhite || s.isEmpty)
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
            board.pieces(index + offset)
          if (if (board.whiteToMove) s.isBlack else s.isWhite || s.isEmpty)
            Some(index + offset)
          else
            None
        }
        .map((index, _))

    val bkCastle =
      if (!board.whiteToMove &&
        board.blackKingCastleAvailable &&
        board.pieces(26).isEmpty &&
        board.pieces(27).isEmpty
      )
        Some((25.toByte, 27.toByte))
      else
        None

    val bqCastle =
      if (!board.whiteToMove &&
        board.blackQueenCastleAvailable &&
        board.pieces(22).isEmpty &&
        board.pieces(23).isEmpty &&
        board.pieces(24).isEmpty
      )
        Some((25.toByte, 23.toByte))
      else
        None

    val wkCastle =
      if (board.whiteToMove &&
        board.whiteKingCastleAvailable &&
        board.pieces(96).isEmpty &&
        board.pieces(97).isEmpty
      )
        Some((95.toByte, 97.toByte))
      else
        None

    val wqCastle =
      if (board.whiteToMove &&
        board.whiteQueenCastleAvailable &&
        board.pieces(92).isEmpty &&
        board.pieces(93).isEmpty &&
        board.pieces(94).isEmpty
      )
        Some((95.toByte, 93.toByte))
      else
        None

    Vector(bkCastle, bqCastle, wkCastle, wqCastle).flatten ++
      board.pieces
        .indices
        // We apply several transformations here, so to avoid the
        // intermediate collections, work from a view instead
        .view
        .filter(index => if (board.whiteToMove) board.pieces(index).isWhite else board.pieces(index).isBlack)
        .flatMap(index =>
          board.pieces(index) match {
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
        .sortBy(move => -board.pieces(move._2).weight)
  }

}
