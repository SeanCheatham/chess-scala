import com.seancheatham.chess.Board
import com.seancheatham.chess._
import org.scalatest.WordSpec

class BoardSpec extends WordSpec {

  "A default chess board" can {
    "be generated" in {
      val board =
        Board.default

      assert(board.pieces(21) == BR && board.pieces(28) == BR)
      assert(board.pieces(22) == BN && board.pieces(27) == BN)
      assert(board.pieces(23) == BB && board.pieces(26) == BB)
      assert(board.pieces(24) == BQ)
      assert(board.pieces(25) == BK)
      assert(board.pieces.slice(31, 39) forall (_ == BP))

      assert(board.pieces(91) == WR && board.pieces(98) == WR)
      assert(board.pieces(92) == WN && board.pieces(97) == WN)
      assert(board.pieces(93) == WB && board.pieces(96) == WB)
      assert(board.pieces(94) == WQ)
      assert(board.pieces(95) == WK)
      assert(board.pieces.slice(81, 89) forall (_ == WP))

    }
  }

  "A chess engine" can {
    "perform alpha/beta search" in {
      val board =
        Board.challenge

      println(board.search(8))
    }
  }

}
