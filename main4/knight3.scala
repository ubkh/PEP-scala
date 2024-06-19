// Finding a single tour on a "mega" board
//=========================================

object M4c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================

import scala.annotation.tailrec

//(1) 
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  x._1 >= 0 && x._1 < dim && x._2 >= 0 && x._2 < dim && !path.contains(x)
}

//(2) 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val potential_moves = List((x._1 + 1, x._2 + 2),(x._1 + 2, x._2 + 1),
                   (x._1 + 2, x._2 - 1),(x._1 + 1, x._2 - 2),
                   (x._1 - 1, x._2 - 2),(x._1 - 2, x._2 - 1),
                   (x._1 - 2, x._2 + 1),(x._1 - 1, x._2 + 2))
  potential_moves.filter(is_legal(dim, path, _))
}

// (3) 
def count_tours(dim: Int, path: Path) : Int = {
  if (path.length < (dim * dim)) legal_moves(dim, path, path.head)
    .map(move => count_tours(dim, move::path))
    .sum
  else 1
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
  if (path.length < (dim * dim)) legal_moves(dim, path, path.head)
    .map(move => enum_tours(dim, move::path))
    .flatten
  else List(path)
}

// (4) 
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  case (a :: b) => f(a) match {
    case None => first(b, f)
    case Some(path) => Some(path)
  }
  case _ => None
}

//(5) 
def first_tour(dim: Int, path: Path) : Option[Path] = {
  if (path.length < (dim * dim)) first(legal_moves(dim, path, path.head), move => first_tour(dim, move::path))
  else Some(path)
}

//(6) 
def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  legal_moves(dim, path, x).sortBy(move => legal_moves(dim, path, move).size)
}


//(7) 
def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
  val moves = ordered_moves(dim, path, path.head)
  val closed_path = first(moves, (move => first_closed_tour_heuristics(dim, move::path)))
  if ((path.length == (dim * dim)) && (legal_moves(dim, List(path.last), path.last).contains(path.head))) Some(path)
  else closed_path
}

//(8)
def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
  val moves = ordered_moves(dim, path, path.head)
  val closed_path = first(moves, (move => first_tour_heuristics(dim, move::path)))
  if (path.length == (dim * dim)) Some(path)
  else closed_path
}

//(9)
def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
  val moves = ordered_moves(dim, path, path.head)
  val closed_path = first(moves, (move => first_tour_heuristics(dim, move::path)))
  if (path.length == (dim * dim)) Some(path)
  else closed_path
}


}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
