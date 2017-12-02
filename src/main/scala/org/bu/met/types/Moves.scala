package org.bu.met.types

import org.bu.met._

trait Moves extends ((Int, Int, Array[Array[Option[ChessPiece]]], Color) => Seq[Position])

object KingMoves extends Moves {
  override def apply(x: Int, y: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[Position] = {
    val possibleMoves =
      Seq((x, y+1), (x, y-1),
          (x+1, y+1), (x+1, y), (x+1, y-1),
          (x-1, y+1), (x-1, y), (x-1, y-1)
      )
      .filter{case (a,b) => range.contains(a) && range.contains(b)}
      .filter{case (a,b) =>
        val (row, col) = toRowCol(a,b)
        board(row)(col) match {
          case Some(piece) => piece.color != color
          case None => true
        }
      }
    possibleMoves
  }
}

object KnightMoves extends Moves {
  override def apply(x: Int, y: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[Position] = {
    val possibleMoves =
      Seq((x+2, y+1), (x+2, y-1),
          (x+1, y+2), (x+1, y-2),
          (x-1, y+2), (x-1, y-2),
          (x-2, y+1), (x-2, y-1)).filter{case (a,b) =>
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
  override def apply(x: Int, y: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[Position] = {
    val yChange = colorValue(color,1)
    val possibleMoves: Seq[Position] = Seq((x, y+yChange)).filter{case (a,b) =>
      val (r,c) = toRowCol(a,b)
      val pieceCondition = board(r)(c) match {
        case Some(_) => false
        case _ => true
      }
      pieceCondition && range.contains(y+yChange)
    }
    val upRight = if(range.contains(x+1) && range.contains(y+yChange)){
      val (r,c) = toRowCol(x+1, y+yChange)
      board(r)(c) match {
        case Some(piece) => if(piece.color != color) Seq((x+1,y+yChange)) else Seq()
        case _ => Seq()
      }
    } else Seq()
    val upLeft = if(range.contains(x-1) && range.contains(y+yChange)){
      val (r,c) = toRowCol(x-1, y+yChange)
      board(r)(c) match {
        case Some(piece) => if(piece.color != color) Seq((x-1, y+yChange)) else Seq()
        case _ => Seq()
      }
    } else Seq()
    val doubleMove = if(y == 1 || y == 6) Seq((x, y + 2*yChange)) else Seq()
    possibleMoves ++ upRight ++ upLeft ++ doubleMove
  }
  private def colorValue(color: Color, yValue: Int): Int = color match{
    case White => yValue
    case Black => -yValue
  }
}

object BishopMoves extends Moves {
  override def apply(x: Int, y: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[Position] = {
    val upRight = generatePositions(board, (x,y), Right, Up, color)
    val upLeft = generatePositions(board, (x,y), LeftDir, Up, color)
    val downRight = generatePositions(board, (x,y), Right, Down, color)
    val downLeft = generatePositions(board, (x,y), LeftDir, Down, color)
    val possibleMoves = upRight ++ upLeft ++ downRight ++ downLeft
    possibleMoves
  }
  protected def generatePositions(board: Board, pos: Position, dirX: Direction, dirY: Direction, color: Color): Seq[Position] = {
    var hitPiece = false
    val (x,y) = pos
    var moves = Seq.empty[Position]
    for(i <- range.drop(1)){ // drop the 0, since moving (0,0) is not moving at al
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
      val (newX, newY) = (x + changeX, y + changeY)
      val (newRow, newCol) = toRowCol(newX, newY)
      val edgeCondition = range.contains(newRow) && range.contains(newCol)
      if(edgeCondition) {
        if (board(newRow)(newCol).nonEmpty) {
          if (board(newRow)(newCol).get.color != color)  // cannot hit own piece
            moves = moves :+ (newX, newY) // can take enemy piece
          hitPiece = true  // cannot move any further if
        }
        if (!hitPiece) moves = moves :+(newX, newY)
      }
    }
    moves
  }
}

object RookMoves extends Moves{
  override def apply(x: Int, y: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[Position] = {
    val right = generatePositions(board, (x,y), Right, color)
    val left = generatePositions(board, (x,y), LeftDir,color)
    val up= generatePositions(board, (x,y), Up, color)
    val down = generatePositions(board, (x,y), Down, color)
    val possibleMoves = right ++ left ++ up ++ down
    possibleMoves
  }
  private def generatePositions(board: Board, pos: Position, dir: Direction, color: Color): Seq[Position] = {
    val (x,y) = pos
    var hitPiece = false
    var moves = Seq.empty[Position]
    for(i <- range.drop(1)){ // drop the 0, since moving (0,0) is not moving at all
      val (changeX, changeY) = dir match {
        case LeftDir => (-i, 0)
        case Right => (i, 0)
        case Up => (0, i)
        case Down => (0, -i)
      }
      val (newX, newY) = (x + changeX, y + changeY)
      val (newRow, newCol) = toRowCol(newX, newY)
      val edgeCondition = range.contains(newRow) && range.contains(newCol)
      if(edgeCondition) {
        if (board(newRow)(newCol).nonEmpty) {
          if (board(newRow)(newCol).get.color != color)  // cannot hit own piece
            moves = moves :+ (newX, newY) // can take enemy piece
          hitPiece = true  // cannot move any further if
        }
        if (!hitPiece) moves = moves :+(newX, newY)
      }
    }
    moves
  }
}

object QueenMoves extends Moves{
  override def apply(x: Int, y: Int, board: Array[Array[Option[ChessPiece]]],color: Color): Seq[Position] =
    RookMoves(x,y,board, color) ++ BishopMoves(x,y,board,color)
}
