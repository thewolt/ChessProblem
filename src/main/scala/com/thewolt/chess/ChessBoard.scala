package com.thewolt.chess

case class Pos(x: Int, y: Int) {
  def withDir(d: Pos) = copy( x = d.x + x , y = d.y + y)
}

case class Data(index: Int, letter: Char) {
  var children : List[Data] = Nil

  def withPiece(pos: Int, letter: Char): Data = {
    val (data, b) = addChild(children, Data(pos, letter))
    if(b) {
      children = data :: children
    }
    data
  }

  /* returns data and ifNew */
  private def addChild(list: List[Data], d: Data): (Data, Boolean) = {
    list match {
      case Nil => (d, true)
      case used :: rest => if(d == used) (used, false) else addChild(rest, d)
    }
  }

}

abstract class Piece(val letter: String) {
  def allowMove(dir: Pos): Boolean
}

object King extends Piece("K") {
  def allowMove(dir: Pos) = (dir.x.abs max dir.y.abs) == 1
}

object Rook extends Piece("R") {
  def allowMove(dir: Pos) = dir.x == 0 || dir.y == 0
}

object Bishop extends Piece("B") {
  def allowMove(dir: Pos) = dir.x.abs == dir.y.abs
}

object Queen extends Piece("Q") {
  def allowMove(dir: Pos) = dir.x.abs == dir.y.abs || dir.x == 0 || dir.y == 0
}

object Knight extends Piece("N") {
  def allowMove(dir: Pos) = dir.x.abs * dir.y.abs == 2
}

class CheckBoard(width: Int, height: Int) {

  class PosWrapper(pos: Pos) {
    def toOffset =  {
      require(pos.x < width && pos.y < height, "position is out of grid")
      pos.y * width + pos.x
    }
    def isInBoard = (0 until width).contains(pos.x)  && (0 until height).contains(pos.y)
  }
  implicit def toPosWrapper(pos: Pos) = new PosWrapper(pos)

  def fromOffset(i: Int) = Pos(i % width, i / width)
  def allPossibleMoves = for( x <- -width to width ; y <- -height to height) yield Pos(x,y)

  case class PieceOnBoard(val piece: Piece) {
    val moves = allPossibleMoves.filter { d => piece.allowMove(d)}
    def letter = piece.letter
  }

  abstract class Square
  case object Empty extends Square
  case object Check extends Square
  case class WithPiece(piece: Piece) extends Square

  case class BoardCons(parent: CheckBoardInstance, piece: PieceOnBoard, pos: Pos)

  class CheckBoardInstance( val layout: Array[Square], info: Option[BoardCons]) {
    require(layout.size == width * height)

    def this() = this(Array.fill(width * height)(Empty), None)
    def this(layout: Array[Square]) = this(layout, None)
    //def this( sqs: Square* ) = this(sqs.toArray)

    lazy val repr : List[(Int, String)] = info match {
      case None => Nil
      case Some(BoardCons(parent, piece, pos)) => (pos.toOffset -> piece.letter) :: parent.repr
    }

    def pieceMoves(piece: PieceOnBoard, pos: Pos) = piece.moves.map { p => pos.withDir(p) }.filter( _.isInBoard )

    def atPos(pos: Pos): Square = layout(pos.toOffset)


    /**
     * gives all possibilities of board with the next piece on board
     */
    def place(piece: PieceOnBoard): Iterable[CheckBoardInstance] = availablePos(piece).flatMap { p =>
      val moves = pieceMoves(piece, p)
      if(_canPieceStay(piece, p, moves)) {
        Some(_withPieceAt(piece, p, moves))
      } else {
        None
      }
    }

    /**
     * means does not check another piece
     * @param piece the piece to place
     * @param pos the position where to place it
     * @return +true+ if the piece does not check another piece, +false+ otherwise
     */
    def canPieceStay(piece: Piece, pos: Pos): Boolean = {
      val pieceOnBoard = PieceOnBoard(piece)
      _canPieceStay(pieceOnBoard, pos, pieceMoves(pieceOnBoard, pos))
    }

    /**
     * construct another board with the piece at pos
     * @param piece the piece to place
     * @param pos the position where to place it
     * @return the new board with the piece in it
     */
    def withPieceAt(piece: Piece, pos: Pos): CheckBoardInstance = {
      val pieceOnBoard = new PieceOnBoard(piece)
      _withPieceAt(pieceOnBoard, pos, pieceMoves(pieceOnBoard, pos))
    }

    def _canPieceStay(piece: PieceOnBoard, pos: Pos, moves: Iterable[Pos]): Boolean = moves.forall { p =>
      val i = atPos(p)
      (i eq Empty) || (i eq Check) 
    }

    def _withPieceAt(piece: PieceOnBoard, pos: Pos, moves: Iterable[Pos]): CheckBoardInstance = {
      val newLayout = layout.clone
      for( p <- moves) {
        newLayout(p.toOffset) = Check
      }
      newLayout.update(pos.toOffset, WithPiece(piece.piece))
      new CheckBoardInstance(newLayout, Some(BoardCons(this, piece, pos)))
    }

    def availablePos: Iterable[Pos] = layout.zipWithIndex.flatMap { case (sq,i) => if(sq == Empty) Some(fromOffset(i)) else None }

    def availablePos(piece: PieceOnBoard): Iterable[Pos] = info match {
      case Some(i) if i.piece == piece => availablePos.filter(p => p.toOffset > i.pos.toOffset)
      case _ => availablePos
    }

    override def equals(obj: Any) = {
      obj match {
        case other: CheckBoardInstance => List(layout: _*) == List(other.layout :_*)
        case _ => false
      }
    }

    override def toString = {
      "Check[\n" + (0 until height).map( y =>
        (0 until width).map( x =>
          atPos(Pos(x,y)) match {
            case Empty => "."
            case Check => "*"
            case WithPiece(p) => p.letter
          }).mkString
      ).mkString("\n") + "\n]"
    }
  }

  object BoardImplicits {
    implicit def toPieceOnBoard(piece: Piece) = PieceOnBoard(piece)
  }

  object EmptyBoard extends CheckBoardInstance()

  def place(_pieces: Piece*) : (Data, Int) = {
    val pieces = Vector( _pieces : _*).map { p => new PieceOnBoard(p) }
    val res = Data(-1, 0)
    var count = 0

    def findPlaces(level: Int, board: CheckBoardInstance) {
      if(level == pieces.size) {
        board.repr.foldLeft(res) { case (data : Data, (pos : Int,letter: String)) => data.withPiece(pos, letter.charAt(0)) }
        count +=1
        if(count % 1000 == 0) {
          println("got res.size="+count)
        }
      } else {
        val curPiece = pieces(level)
        board.place(curPiece).foreach( newBoard => findPlaces(level+1, newBoard) )
      }
    }
    findPlaces(0, EmptyBoard)
    (res, count)
  }
}


object Main {
  def main(args: Array[String]) {
    val board = new CheckBoard(7, 7)
    val (res, count) = board.place(Queen, Queen, Bishop, Bishop, King, King, Knight)
    println( count )
  }
}