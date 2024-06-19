// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

// (1)
def load_bff(name: String) : String = {
  Try(Source.fromFile(name).mkString).getOrElse("")
}

// (2) 

def sread(mem: Mem, mp: Int) : Int = mem.getOrElse(mp, 0)

def write(mem: Mem, mp: Int, v: Int) : Mem = mem + (mp -> v)

// (3) 

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
  if (pc >= prog.length || level < 0) pc
  else prog(pc) match {
    case '[' => jumpRight(prog, pc + 1, level + 1)
    case ']' => {
        if (level == 0) pc + 1
        else jumpRight(prog, pc + 1, level - 1)
    }
    case _ => jumpRight(prog, pc + 1, level)
  }
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
  if (pc < 0 || level < 0) pc
  else prog(pc) match {
      case ']' => jumpLeft(prog, pc - 1, level + 1)
      case '[' => {
          if (level == 0) pc + 1
          else jumpLeft(prog, pc - 1, level - 1)
      }
      case _ => jumpLeft(prog, pc - 1, level)
  }   
}


// testcases
//jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
//jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
//jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
//jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
//jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
//jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



// (4) 

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc >= prog.length) mem
  else prog(pc) match {
    case '+' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
    case '-' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
    case '>' => compute(prog, pc + 1, mp + 1, mem)
    case '<' => compute(prog, pc + 1, mp - 1, mem)
    case '.' => compute(prog, pc + 1, mp, mem)
    case '[' => {
        if (sread(mem, mp) == 0) compute(prog, jumpRight(prog, pc + 1, 0), mp, mem)
        else compute(prog, pc + 1, mp, mem)
    }
    case ']' => {
        if (sread(mem, mp) == 0) compute(prog, pc + 1, mp, mem)
        else compute(prog, jumpLeft(prog, pc - 1, 0), mp, mem)
    }
    case _ => compute(prog, pc + 1, mp, mem)
    }
}

def run(prog: String, m: Mem = Map()) = compute(prog, 0, 0, m)

// (5)
def generate(msg: List[Char]) : String = msg match {
  case Nil => ""
  case a :: b => {
    val ascii = a.toInt
    "+" * ascii + ".[-]" + generate(b)
  }
}

// (6) 
def jtable(pg: String, pc: Int = 0, mem: Map[Int, Int] = Map()) : Map[Int, Int] = {
  if (pc >= pg.length) mem
  else pg(pc) match {
    case '[' => jtable(pg, pc + 1, mem + (pc -> jumpRight(pg, pc + 1, 0)))
    case ']' => jtable(pg, pc + 1, mem + (pc -> jumpLeft(pg, pc - 1, 0)))
    case _ => jtable(pg, pc + 1, mem)
  }
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc >= pg.length) mem
  else pg(pc) match {
    case '+' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
    case '-' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
    case '>' => compute2(pg, tb, pc + 1, mp + 1, mem)
    case '<' => compute2(pg, tb, pc + 1, mp - 1, mem)
    case '.' => compute2(pg, tb, pc + 1, mp, mem)
    case '[' => {
        if (sread(mem, mp) == 0) compute2(pg, tb, tb(pc), mp, mem)
        else compute2(pg, tb, pc + 1, mp, mem)
    }
    case ']' => {
        if (sread(mem, mp) == 0) compute2(pg, tb, pc + 1, mp, mem)
        else compute2(pg, tb, tb(pc), mp, mem)
    }
    case _ => compute2(pg, tb, pc + 1, mp, mem)
    }
}

def run2(pg: String, m: Mem = Map()) = compute2(pg, jtable(pg), 0, 0, m)

// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (7) 

def optimise(s: String) : String = {
  s.replaceAll("""[^<>+-.,\[\]]""", "").replaceAll("""\[-\]""", "0")
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc >= pg.length) mem
  else pg(pc) match {
    case '+' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
    case '-' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
    case '>' => compute3(pg, tb, pc + 1, mp + 1, mem)
    case '<' => compute3(pg, tb, pc + 1, mp - 1, mem)
    case '.' => compute3(pg, tb, pc + 1, mp, mem)
    case '[' => {
        if (sread(mem, mp) == 0) compute3(pg, tb, tb(pc), mp, mem)
        else compute3(pg, tb, pc + 1, mp, mem)
    }
    case ']' => {
        if (sread(mem, mp) == 0) compute3(pg, tb, pc + 1, mp, mem)
        else compute3(pg, tb, tb(pc), mp, mem)
    }
    case '0' => compute3(pg, tb, pc + 1, mp, write(mem, mp, 0))
    case _ => compute3(pg, tb, pc + 1, mp, mem)
    }
}

def run3(pg: String, m: Mem = Map()) = compute3(pg, jtable(pg), 0, 0, m)


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11203
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (8)  
def combine(s: String) : String = ???

// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ???

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = ???


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
