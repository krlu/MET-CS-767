package org.bu.met

import java.io.FileWriter
import org.bu.met.types._
import scala.io.Source

class ChessGame(var activePieces: Seq[(ChessPiece, Position)], var turn: Color){

  // training data we shall save in csv form
  private var moveVectorOpt: Option[MoveVector] = None
  private var stateVectorOpt: Option[Vector] = None
  private var gameOver: Boolean = false
  private var kingInCheck: Boolean = false
  var numMoves = 0

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

  def runGame(maxMoves: Int, model: Option[InferenceModel] = None): Unit ={
    while(!gameOver && numMoves < maxMoves) {
      updateBoard(model)
      numMoves += 1
    }
  }

  def updateBoard(inferenceModel: Option[InferenceModel] = None): Unit = {
    // tuple contains (selectedPiece, oldPosition, new position)
    kingInCheck = updateKingCheck(turn)
    val tupleOpt = inferenceModel match {
      case Some(model) => selectMoveWithNeuralNet(model)
      case None => randomlySelectMove()
    }
    tupleOpt match {
      case Some((selectedPiece, oldPos, newPos)) =>
        updateBoardHelper(selectedPiece, oldPos, newPos, kingInCheck)
      case None =>
        if(kingInCheck){
          gameOver = true
          saveMoveAndState(moveVectorOpt, stateVectorOpt)
          val opposingColor = if(turn == White) Black else White
//          println(s"$opposingColor wins!!!")
        }
        else {
          gameOver = true
//          println(s"stalemate, $turn cannot move.")
        }
    }
    turn = if (turn.equals(White)) Black else White // switch turns
  }

  private def selectMoveWithNeuralNet(model: InferenceModel): Option[(ChessPiece, Position, Position)] = {
    val moveVector = model.computeMoveVector(StateVector(activePieces, turn))
    val (selectedPieceOpt, newPos) = moveVector.toReadableMove
    selectedPieceOpt match {
      case Some(selectedPiece) =>
        activePieces.find{case(p, _) => p == selectedPiece} match {
          case Some((piece, oldPos)) =>
            val possibleMoves = getPossibleMoves(selectedPiece, oldPos)
            if(possibleMoves.contains(newPos)) Some((piece, oldPos, newPos)) else randomlySelectMove()
          case _ => randomlySelectMove()
        }
      case _ => randomlySelectMove()
    }
  }

  private def randomlySelectMove(): Option[(ChessPiece, Position, Position)] = {
    val piecesToMove: Seq[(ChessPiece, Position)] = activePieces.filter{case(piece, _) => piece.color == turn}
    val possibleMoves = piecesToMove.flatMap{ case (piece, (x,y)) =>
      getPossibleMoves(piece, (x, y)).map{ move => (piece,(x,y),move)}
    }
    if(possibleMoves.isEmpty) None
    else {
      val kingOpt: Option[(ChessPiece, (Int, Int))] = piecesToMove.find { case (piece, _) => piece.isInstanceOf[King] }
      val tupleOpt = kingOpt match {
        case Some((king: King, (x, y))) =>
          if (kingInCheck){
            val kingMoves = possibleMoves.filter(_._1 == king)
            if(kingMoves.isEmpty) None else Some(choose(kingMoves.iterator))
          }
          else Some(choose(possibleMoves.iterator))
        case _ => Some(choose(possibleMoves.iterator))
      }
      tupleOpt
    }
  }
  private def updateKingCheck(color: Color): Boolean ={
    val piecesToMove: Seq[(ChessPiece, Position)] = activePieces.filter{case(piece, _) => piece.color == turn}
    val kingOpt: Option[(ChessPiece, (Int, Int))] = piecesToMove.find { case (piece, _) => piece.isInstanceOf[King] }
    kingOpt match {
      case Some((king: King, (x, y))) => inCheck(x, y, board, activePieces, turn)
      case _ => false
    }
  }
  private def getPossibleMoves(selectedPiece: ChessPiece, oldPos: Position): Seq[Position] ={
    val (oldX, oldY) = oldPos
    selectedPiece match {
      case _: King => getMovesForPiece(selectedPiece, oldX, oldY, board).filter{case (a, b) =>
        val(oldRow, oldCol) = toRowCol(oldX,oldY)
        val(newRow, newCol) = toRowCol(a,b)
        val hypotheticalBoard = deepCopyBoard
        val hypotheticalPieces = hypotheticalBoard(newRow)(newCol) match {
          case Some(piece) => activePieces.filter{case (p, __) => p.stateVectorIndex != piece.stateVectorIndex}
          case None => activePieces
        }
        hypotheticalBoard(oldRow)(oldCol) = None
        hypotheticalBoard(newRow)(newCol) = Some(selectedPiece)
        !inCheck(a,b,hypotheticalBoard,hypotheticalPieces,turn)
      }
      case _ => getMovesForPiece(selectedPiece, oldX, oldY, board)
    }
  }
  private def updateBoardHelper(selectedPiece: ChessPiece, oldPosition: Position,
                        newPosition: Position, kingInCheck: Boolean): Unit ={

      // if new state vector is in the training set, then save the state and move
      // save state before updating stateVectorOpt
      // TODO: throw out old state vectors at some point
      val newSaveState = StateVector(activePieces, turn)
      val matchingState = getTrainingStates("training_data.csv").find{case(s, _) => s == newSaveState}
      if(matchingState.nonEmpty)
        saveMoveAndState(moveVectorOpt, stateVectorOpt)
      val (oldX, oldY) = oldPosition
      val (newX, newY) = newPosition
      stateVectorOpt = Some(newSaveState)
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
    println()
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
