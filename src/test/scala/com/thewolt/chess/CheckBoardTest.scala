package com.thewolt.chess

import org.scalatest._

/**
 * Created by IntelliJ IDEA.
 * User: thewolt
 * Date: Nov 14, 2010
 * Time: 9:57:32 PM
 * To change this template use File | Settings | File Templates.
 */

class CheckBoardTest extends FunSuite {
  val board = new CheckBoard(3, 3)
  import board._
  import board.BoardImplicits._

  test("board instance should list available pos") {
    val i = new CheckBoardInstance( Array(
      WithPiece(King), Check, Empty,
      Check, Check, Empty,
      Empty, Empty, Empty
    ))
    assert( i.availablePos.toSet === Set(Pos(2, 0), Pos(2, 1), Pos(0, 2), Pos(1, 2), Pos(2, 2)))
  }

  test("board instance should generate an instance with another piece") {
    val i = new CheckBoardInstance()
    assert( i.withPieceAt(King, Pos(0, 0)) === new CheckBoardInstance(Array(
        WithPiece(King), Check, Empty,
        Check, Check, Empty,
        Empty, Empty, Empty
      )))
  }

  test("board instance should give all move of a piece") {
    val i = new CheckBoardInstance()
    assert( i.pieceMoves(King, Pos(0, 0)).toSet === Set(Pos(0, 1), Pos(1, 1), Pos(1, 0)))
  }

  test("board instance should tell if a piece can stay") {
    val i = (new CheckBoardInstance).withPieceAt(King, Pos(0, 0))
    assert( i.canPieceStay(Rook, Pos(2, 0)) === false)
    assert( i.canPieceStay(Rook, Pos(2, 1)) === true )
  }

  test("board instance can give a list of all possible place for a piece") {
    val i = (new CheckBoardInstance).withPieceAt(King, Pos(0, 0))
    assert( i.place(Rook).toSet === Set(Pos(2, 1), Pos(2, 2), Pos(1, 2)).map( i.withPieceAt(Rook, _)))
  }
}

class CheckBoardFunctionalTest extends FunSuite {
  test("board can place 2 king and a rook") {
    implicit val board = new CheckBoard(3, 3)

    val (places, count) = board.place(King, King, Rook)
    assert( count === 4 )
    info(places)
  }

  test("board can place 8 queens") {
    implicit val board = new CheckBoard(8, 8)

    val (places, count) = board.place((1 to 8).map(_ => Queen).toList : _*)
    assert( count === 92)
    info(places)
  }

  def info(places: List[List[Data]])(implicit b: CheckBoard) {
    import b._
    println( places.map { l => l.foldLeft(EmptyBoard: CheckBoardInstance) { (board, data) => board.withPieceAt(data.piece, data.pos) } }.mkString("\n"))
  }
}
