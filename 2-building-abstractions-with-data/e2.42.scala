object NQueens extends App {

  class Position(val row: Int, val col: Int) {
    override def toString = s"($row, $col)"
  }

  def queens(boardSize: Int): List[List[Position]] = {

    def isSafe(positions: List[Position]): Boolean = positions match {
      case(newPosition :: existingPositions) => {
        existingPositions.forall { position => position.row != newPosition.row } &&
        existingPositions.forall { position => position.col != newPosition.col } &&
        existingPositions.forall { position => Math.abs(position.row - newPosition.row) != Math.abs(position.col - newPosition.col) }
      }
      case(Nil) => true
    }

    def positionsForCol(k: Int): List[Position] = {
      ((1 to boardSize) map { (row: Int) =>
        new Position(row, k)
      }).toList
    }
    def queenCols(k: Int): List[List[Position]] = {
      if (k == 0) {
        List(List())
      }
      else {
        val candidates = queenCols(k - 1) flatMap ( kMinusOneSolution => positionsForCol(k) map { _ ::  kMinusOneSolution } )
        candidates filter { isSafe(_) }
      }
    }
    queenCols(boardSize)
  }
}
