// Main Part 4 about finding Knight's tours
//==========================================


object M4a {

// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end of the file are of any help.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================



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


//some testcases
//
//assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


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


// testcases
//
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) 
def first_tour(dim: Int, path: Path) : Option[Path] = {
  if (path.length < (dim * dim)) first(legal_moves(dim, path, path.head), move => first_tour(dim, move::path))
  else Some(path)
}



/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//
//     time_needed(count_tours(dim, List((0, 0))))
//
// in order to print out the time that is needed for 
// running count_tours


// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println()
  } 
}


*/

}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
