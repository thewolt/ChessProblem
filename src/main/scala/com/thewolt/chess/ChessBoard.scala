package com.thewolt.chess

import collection.mutable.Stack
import collection.mutable.Set

trait IPos {
  type P <: IPos
  val x: Int
  val y: Int
  def withDir(d: IPos): P
  def toOffset: Int
}

trait Piece {
  val moves: Iterable[IPos]
  val letter: String
}

object Data {
    /* returns data and found */
  def addChild(children: List[Data], d: Data): (Data, Boolean) = {
    children match {
      case Nil => (d, true)
      case used :: rest => if(d == used) (used, false) else addChild(rest, d)
    }
  }
}

case class Data(index: Int, letter: Char) {
  var children : List[Data] = Nil

  def withPiece2(pos: IPos, letter: String): (Data,Boolean) = {
    withPiece(pos.toOffset, letter.charAt(0))
  }
  def withPiece(pos: Int, letter: Char): (Data,Boolean) = {
    val (data, b) = Data.addChild(children, Data(pos, letter))
    if(b) {
      children = data :: children
    }
    (data,b)
  }
}

class CheckBoard(width: Int, height: Int) {
  import math._

  case class Pos(x: Int, y: Int) extends IPos {
    type P = Pos
    def withDir(d: IPos) = copy( x = d.x + x , y = d.y + y)
    def toOffset = y * width + x
  }

  def fromOffset(i: Int) = Pos(i % width, i / width)
  def allPossibleMoves = for( x <- -width to width ; y <- -height to height) yield Pos(x,y)


  object King extends Piece {
    val moves = allPossibleMoves.filter { p => max(abs(p.x), abs(p.y)) == 1 }
    val letter = "K"
  }

  object Rooke extends Piece {
    val moves = allPossibleMoves.filter { p => p.x == 0 || p.y == 0 }
    val letter = "R"
  }

  object Bishop extends Piece {
    val moves = allPossibleMoves.filter { p => abs(p.x) == abs(p.y) }
    val letter = "B"
  }

  object Queen extends Piece {
    val moves = allPossibleMoves.filter { p => abs(p.x) == abs(p.y) || p.x == 0 || p.y == 0 }
    val letter = "Q"
  }

  object Knight extends Piece {
    val moves = allPossibleMoves.filter { p => abs(p.x) * abs(p.y) == 2 }
    val letter = "N"
  }

  abstract class Square
  case object Empty extends Square
  case object Check extends Square
  case class WithPiece(p: Piece) extends Square

  case class BoardCons(parent: CheckBoardInstance, piece: Piece, pos: Pos)

  class CheckBoardInstance( val layout: Array[Square], info: Option[BoardCons]) {
    require(layout.size == width * height)

    def this() = this(Array.fill(width * height)(Empty), None)
    def this(layout: Array[Square]) = this(layout, None)
    //def this( sqs: Square* ) = this(sqs.toArray)

    lazy val repr : List[(Int, String)] = info match {
      case None => Nil
      case Some(BoardCons(parent, piece, pos)) => (pos.toOffset -> piece.letter) :: parent.repr
    }

    def usedRepr = {
      val buf = new StringBuilder
      repr.toList.sortBy(a => a._1).foreach { case (k,v) => buf.append(k).append(v) }
      buf.toString
    }

    val inBoard: Pos => Boolean = (p: Pos) => p.x >= 0 && p.x < width && p.y >=0 && p.y < height

    def pieceMoves(piece: Piece, pos: Pos) = piece.moves.map { p => pos.withDir(p) }.filter( inBoard )

    def atPos(pos: Pos): Square = layout(pos.toOffset)

    def withPieceAt(piece: Piece, pos: Pos): CheckBoardInstance = {
      _withPieceAt(piece, pos, pieceMoves(piece, pos))
    }

    def _withPieceAt(piece: Piece, pos: Pos, moves: Iterable[Pos]): CheckBoardInstance = {
      val newLayout = layout.clone
      for( p <- moves) {
        newLayout(p.toOffset) = Check
      }
      newLayout.update(pos.toOffset, WithPiece(piece))
      new CheckBoardInstance(newLayout, Some(BoardCons(this, piece, pos)))
    }

    def place(piece: Piece): Iterable[CheckBoardInstance] = availablePos(piece).flatMap { p =>
      val moves = pieceMoves(piece, p)
      if(_canPieceStay(piece, p, moves)) {
        Some(_withPieceAt(piece, p, moves))
      } else {
        None
      }
    }

    /**
     * means does not check another piece
     */
    def canPieceStay(piece: Piece, pos: Pos): Boolean = _canPieceStay(piece, pos, pieceMoves(piece, pos))

    def _canPieceStay(piece: Piece, pos: Pos, moves: Iterable[Pos]): Boolean = moves.forall { p =>
      val i = atPos(p)
      (i eq Empty) || (i eq Check) 
    }

    def availablePos: Iterable[Pos] = layout.zipWithIndex.flatMap { case (sq,i) => if(sq == Empty) Some(fromOffset(i)) else None }

    def availablePos(piece: Piece): Iterable[Pos] = info match {
      case Some(i) if i.piece == piece => availablePos.filter(p => p.toOffset > i.pos.toOffset)
      case _ => availablePos
    }

    override def equals(obj: Any) = {
      obj match {
        case other: CheckBoardInstance => List(layout: _*) == List(other.layout :_*)
        case _ => false
      }
    }

    override def toString = "Check[\n" + (0 until height).map( y =>
      (0 until width).map( x =>
        atPos(Pos(x,y)) match {
          case Empty => "."
          case Check => "*"
          case WithPiece(p) => p.letter
        }).mkString
      ).mkString("\n") + "\n]"
  }

  object EmptyBoard extends CheckBoardInstance()

  case class Level( level: Int, cur: CheckBoardInstance)

  def place(_pieces: Piece*): (List[Data], Int) = {
    val pieces = Vector( _pieces : _*)
    val stack = new Stack[Level]()
    stack.push(Level(0, EmptyBoard))
    var count = 0
    var res = List[Data]()
    while( !stack.isEmpty) {
      val Level(level, board) = stack.pop

      if(level == pieces.size) {
        val root :: rest = board.repr
        val (resRoot, resRootAdd) = Data.addChild(res, Data(root._1, root._2.charAt(0)))
        if(resRootAdd) {
          res ::= resRoot
        }
        val (_, newBranch) = rest.foldLeft((resRoot, false)) { case ((data : Data,b : Boolean), (pos : Int,letter: String)) =>
          val (newData, newB) = data.withPiece(pos, letter.charAt(0))
          (newData , newB || b)
        }
        if(newBranch) {
          count += 1
        }
        if(count % 1000 == 0) {
          println("got res.size="+count)
        }
      } else {
        val curPiece = pieces(level)
        board.place(curPiece).foreach( newBoard =>
          stack.push(Level(level+1, newBoard))
        )
      }
    }
    (res,count)
  }

}




object Main {
  def main(args: Array[String]) {
    val board = new CheckBoard(7, 7)
    import board._

    val (res, count) = board.place(Queen, Queen, Bishop, Bishop, King, King, Knight)
    println( count )
  }
}