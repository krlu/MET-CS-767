package org.bu.met

import org.bu.met.types._

trait Moves extends ((Int, Int, Array[Array[Option[ChessPiece]]], Color) => Seq[(Int, Int)])

object KingMoves extends Moves {
  override def apply(row: Int, col: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[(Int, Int)] = {
    val possibleMoves =
      Seq((row, col+1), (row, col-1),
          (row+1, col+1), (row+1, col), (row+1, col-1),
          (row-1, col+1), (row-1, col), (row-1, col-1)).filter{case (a,b) =>
        val range = 0 to 7
        val edgeCondition = range.contains(a) && range.contains(b)
        val pieceCondition = board(b)(a) match {
          case Some(piece) => piece.color != color
          case _ => true
        }
        edgeCondition && pieceCondition
      }
    possibleMoves
  }
}

object KnightMoves extends Moves {
  override def apply(row: Int, col: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[(Int, Int)] = {
    val possibleMoves =
      Seq((row+2, col+1), (row+2, col-1),
          (row+1, col+2), (row+1, col-2),
          (row-1, col+2), (row+1, col-2),
          (row-2, col+1), (row+2, col-1)).filter{case (a,b) =>
        val edgeCondition = range.contains(a) && range.contains(b)
        val pieceCondition = board(b)(a) match {
          case Some(piece) => piece.color != color
          case _ => true
        }
        edgeCondition && pieceCondition
      }
    possibleMoves
  }
}

object PawnMoves extends Moves {
  override def apply(row: Int, col: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[(Int, Int)] = {
    val yChange = col + colorValue(color,1)
    val possibleMoves: Seq[(Int, Int)] = Seq((row, col+yChange)).filter{case (a,b) =>
      val pieceCondition = board(b)(a) match {
        case Some(piece) => piece.color != color
        case _ => true
      }
      pieceCondition && range.contains(col+yChange)
    }
    val upRight = if(range.contains(row+1) && range.contains(col+yChange)){
      board(row+1)(col+1) match {
        case Some(piece) => if(piece.color != color) Seq((row-1,col+yChange)) else Seq()
        case _ => Seq()
      }
    } else Seq()
    val upLeft = if(range.contains(row-1) && range.contains(col+yChange)){
      board(row-1)(col+1) match {
        case Some(piece) => if(piece.color != color) Seq((row-1, col+yChange)) else Seq()
        case _ => Seq()
      }
    } else Seq()
    val doubleMove = if(col == 1 || col == 6) Seq((row, col + 2*yChange)) else Seq()
    possibleMoves ++ upRight ++ upLeft ++ doubleMove
  }
  private def colorValue(color: Color, yValue: Int): Int = color match{
    case White => yValue
    case Black => -yValue
  }
}

object BishopMoves extends Moves {
  override def apply(row: Int, col: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[(Int, Int)] = {
    val upRight = generatePositions(board, (row,col), Right, Up, color)
    val upLeft = generatePositions(board, (row,col), LeftDir, Up, color)
    val downRight = generatePositions(board, (row,col), Right, Down, color)
    val downLeft = generatePositions(board, (row,col), LeftDir, Down, color)
    val possibleMoves = upRight ++ upLeft ++ downRight ++ downLeft
    possibleMoves
  }
  protected def generatePositions(board: Board, pos: (Int, Int), dirX: Direction, dirY: Direction, color: Color): Seq[(Int, Int)] = {
    var hitPiece = false
    val (row,col) = pos
    var moves = Seq.empty[(Int, Int)]
    for(i <- range){
      val changeX = dirX match {
        case LeftDir => -i
        case Right => i
        case _ => throw new IllegalArgumentException("dirX must be left or right")
      }
      val changeY = dirY match {
        case Up => -i
        case Down => i
        case _ => throw new IllegalArgumentException("dirY must be up or down")
      }
      val (newX, newY) = (row + changeX, col + changeY)
      val edgeCondition = range.contains(newX) && range.contains(newY)
      if(edgeCondition)
        if(board(newX)(newY).nonEmpty && board(newX)(newY).get.color == color)
          hitPiece = true // cannot hit own piece
      if(!hitPiece)
        moves = moves :+ (newX, newY)
      if(board(newX)(newY).nonEmpty)
        hitPiece = true // cannot move any further if hit enemy piece
    }
    moves
  }
}

object RookMoves extends Moves{
  override def apply(row: Int, col: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[(Int, Int)] = {
    val right = generatePositions(board, (row,col), Right, color)
    val left = generatePositions(board, (row,col), LeftDir,color)
    val up= generatePositions(board, (row,col), Up, color)
    val down = generatePositions(board, (row,col), Down, color)
    val possibleMoves = right ++ left ++ up ++ down
    possibleMoves
  }
  private def generatePositions(board: Board, pos: (Int, Int), dir: Direction, color: Color): Seq[(Int, Int)] = {
    val (row,col) = pos
    var hitPiece = false
    var moves = Seq.empty[(Int, Int)]
    for(i <- range){
      val (changeX, changeY) = dir match {
        case LeftDir => (-i, 0)
        case Right => (i, 0)
        case Up => (0, i)
        case Down => (0, -i)
      }
      val (newX, newY) = (row + changeX, col + changeY)
      val edgeCondition = range.contains(newX) && range.contains(newY)
      if(edgeCondition)
        if(board(newX)(newY).nonEmpty && board(newX)(newY).get.color == color)
          hitPiece = true // cannot hit own piece
      if(!hitPiece)
        moves = moves :+ (newX, newY)
      if(board(newX)(newY).nonEmpty)
        hitPiece = true // cannot move any further if hit enemy piece
    }
    moves
  }
}

object QueenMoves extends Moves{
  override def apply(row: Int, col: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[(Int, Int)] =
    RookMoves(row,col,board, color) ++ BishopMoves(row,col,board,color)
}
