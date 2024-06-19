// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// ADD YOUR CODE BELOW
//======================


// (1) 
def is_op(op: String) : Boolean = ops.contains(op)

def prec(op1: String, op2: String) : Boolean = precs(op1) <= precs(op2)

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
  case Nil => if (st.isEmpty) out else syard(Nil, st.tail, out :+ st.head)
  case h :: t if is_op(h) => st match {
	case op :: _ if (is_op(op) && prec(h, op)) => syard(toks, st.tail, out :+ st.head)
	case _ => syard(t, h :: st, out)
  }
  case "(" :: t => syard(t, "(" :: st, out)
  case ")" :: t => st match {
	case "(" :: _ => syard(t, st.tail, out)
	case t :: _ => syard(toks, st.tail, out :+ t)
	case Nil => throw new IllegalArgumentException("Invalid parenthesis structure")
  }
  case num :: t if (num forall Character.isDigit) => syard(t, st, out ::: List(num))
  case _ => throw new IllegalArgumentException("Invalid token")
}

// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2)
def apply_op(op: String, a: Int, b: Int) : Int = op match {
  case "+" => a + b
  case "-" => a - b
  case "*" => a * b
  case "/" => a / b
}

def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match {
  case Nil => st.head
  case op :: t if is_op(op) => {
	val (n2 :: n1 :: remaining) = st
	compute(t, apply_op(op, n1, n2) :: remaining)
  }
  case num :: t => compute(t, num.toInt :: st)
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

