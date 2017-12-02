package org.bu.met

import java.io.FileWriter
import org.bu.met.types._
import scala.io.Source

class ChessGame(var activePieces: Seq[(ChessPiece, Position)], var turn: Color){

  // training data we shall save in csv form
  private var moveVectorOpt: Option[MoveVector] = None
  private var stateVectorOpt: Option[Vector] = None
  private var gameOver: Boolean = false
//  val model = new InferenceModel
//  model.train("old_training_data.csv")

  activePieces.foreach{case(_, (row, col)) =>
    require(range.contains(row) && range.contains(col))
  }

  val board: Board = {
    val board: Board = Array.fill(8)(Array.fill(8)(None))
    activePieces.foreach{case(piece, (x,y))=>
      val (row,col) = toRowCol(x,y)
      board(row)(col) = Some(piece)
    }
    board
  }

  def runGame(maxMoves: Int): Unit ={
    var numMoves = 0
    while(!gameOver && numMoves < maxMoves) {
      updateBoard()
      numMoves += 1
    }
  }

  def updateBoard(): Unit = {

    val piecesToMove: Seq[(ChessPiece, Position)] = activePieces.filter{case(piece, _) => piece.color == turn}
    println(s"Game State: $activePieces, $turn")
//    if(activePieces.count { case (piece, _) => piece.isInstanceOf[King] } != 2)
//      throw new IllegalStateException("Must have 2 kings!!!")
//    val moveVector: MoveVector = model.computeMoveVector(StateVector(activePieces, turn))

    val kingOpt: Option[(ChessPiece, (Int, Int))] = piecesToMove.find{case (piece, _) => piece.isInstanceOf[King]}
    var kingInCheck = false
    val (selectedPiece: ChessPiece, (oldX,oldY)) = kingOpt match {
      case Some((king: King, (x,y))) =>
        kingInCheck = inCheck(x,y,board,activePieces,turn)
        if(kingInCheck) (king, (x,y))
        else choose(piecesToMove.iterator)
      // in some test cases there are no kings, but this wouldn't be realistic
      case _ => choose(piecesToMove.iterator)
    }
    val possibleMoves: Seq[Position] = selectedPiece match {
      case _: King => getMovesForPiece(selectedPiece, oldX, oldY, board).filter{case (a, b) =>
        val(oldRow, oldCol) = toRowCol(oldX,oldY)
        val(newRow, newCol) = toRowCol(a,b)
        val hypotheticalBoard = deepCopyBoard
        hypotheticalBoard(oldRow)(oldCol) = None
        hypotheticalBoard(newRow)(newCol) = Some(selectedPiece)
        !inCheck(a,b,hypotheticalBoard,activePieces,turn)
      }
      case _ => getMovesForPiece(selectedPiece, oldX, oldY, board)
    }
    if(possibleMoves.nonEmpty) {

      // if new state vector is in the training set, then save the state and move
      // save state before updating stateVectorOpt
      // TODO: throw out old state vectors at some point
      val newState = StateVector(activePieces, turn)
      val matchingState = getTrainingStates("training_data.csv").find{case(s, _) => s == newState}
      if(matchingState.nonEmpty)
        saveMoveAndState(moveVectorOpt, stateVectorOpt)
      val (newX, newY) = choose(possibleMoves.iterator)
      stateVectorOpt = Some(newState)
      moveVectorOpt = Some(MoveVector(selectedPiece.stateVectorIndex, newX, newY))
      val (newRow, newCol) = toRowCol(newX, newY)
      val (oldRow, oldCol) = toRowCol(oldX, oldY)
      activePieces = activePieces.filter { case (_, (x, y)) => x != oldX || y != oldY }
      if (board(newRow)(newCol).nonEmpty)
        activePieces = activePieces.filter { case (_, (x, y)) => x != newX || y != newY }
      selectedPiece match {
        case p: Pawn => attemptPromotePawn(p, (oldX, oldY))
        case _ =>
      }
      activePieces = activePieces :+(selectedPiece, (newX, newY))
      board(newRow)(newCol) = Some(selectedPiece)
      board(oldRow)(oldCol) = None
    }
    else if(kingInCheck){
      gameOver = true
      saveMoveAndState(moveVectorOpt, stateVectorOpt)
      val opposingColor = if(turn == White) Black else White
      println(s"$opposingColor wins!!!")
    }
    else {
      gameOver = true
      println(s"stalemate, $turn cannot move.")
    }
    turn = if (turn.equals(White)) Black else White // switch turns
  }

  private def getTrainingStates(fileName: String): Seq[(StateVector, MoveVector)] ={
    val bufferedSource = Source.fromFile(fileName)
    var states = Seq.empty[(StateVector, MoveVector)]
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim.toInt)
      states = states :+ (StateVector(cols.dropRight(3).toSeq), MoveVector(cols(97), cols(98), cols(99)))
    }
    states
  }

  private def saveMoveAndState(moveVectorOpt: Option[MoveVector], stateVectorOpt: Option[Vector]): Unit ={
    (stateVectorOpt, moveVectorOpt) match {
      case (Some(sv: Vector), Some(mv: MoveVector)) =>
        if(!getTrainingStates("training_data.csv").exists{case(s, m) => (s == sv) && m == mv}){
          val fw = new FileWriter("training_data.csv", true)
          fw.write(s"$sv,$mv\n")
          fw.close()
        }
      case _ =>
    }
  }

  private def attemptPromotePawn(p: Pawn, position: Position): Unit ={
    def promotablePawn(col: Int, color: Color) =
      (color == White && col == 7) || (color == Black && col == 0)
    val (row,col) = position

    if(promotablePawn(col, p.color)){
      val possiblePieces = List(Knight(p.color, p.stateVectorIndex), Queen(p.color, p.stateVectorIndex))
      val promotedPiece = choose(possiblePieces.iterator)
      board(row)(col) = Some(promotedPiece)
      activePieces = activePieces.filter{case( piece, pos) => piece match {
        case _: Pawn => pos == (row,col)
        case _ => false
      }}
      activePieces = activePieces :+ ((promotedPiece, (row,col)))
    }
  }

  def printBoard(): Unit ={
    for(row <- range){
      println()
      for(col <- range) {
        board(row)(col) match {
          case None => print("[            ]")
          case Some(piece) => print(s"[$piece]")
        }
      }
    }
  }
  def isGameOver: Boolean = gameOver

  def deepCopyBoard: Board = {
    val newBoard: Array[Array[Option[ChessPiece]]] = Array.fill(8)(Array.fill(8)(None))
    activePieces.foreach{case(piece, (x,y))=>
      val (row,col) = toRowCol(x,y)
      newBoard(row)(col) = Some(piece)
    }
    newBoard
  }
}
