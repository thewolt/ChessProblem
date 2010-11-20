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

  test("board can place 2 king and a rook") {
    assert( board.place(King, King, Rook)._2 === 4)
//    assert( board.place(King, King, Rook) === Set(
//      EmptyBoard.withPieceAt(King, Pos(0, 0)).withPieceAt(King, Pos(0, 2)).withPieceAt(Rooke, Pos(2, 1)),
//      EmptyBoard.withPieceAt(King, Pos(0, 0)).withPieceAt(King, Pos(2, 0)).withPieceAt(Rooke, Pos(1, 2)),
//      EmptyBoard.withPieceAt(King, Pos(0, 2)).withPieceAt(King, Pos(2, 2)).withPieceAt(Rooke, Pos(1, 0)),
//      EmptyBoard.withPieceAt(King, Pos(2, 0)).withPieceAt(King, Pos(2, 2)).withPieceAt(Rooke, Pos(0, 1))
//
//      ).map(_.usedRepr) )
  }
}