// Main Part 2 about Evil Wordle
//===============================


object M2 { 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


//(1)
def get_wordle_list(url: String) : List[String] = {
  val regex = "^(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|].txt$".r
  val matching = regex.findFirstMatchIn(url)
  if (matching.isEmpty) Nil
  else {
    val file = Source.fromURL(url)
    val data = for (line <- file.getLines()) yield line
    data.toList
  }
}

// val secrets = get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
// secrets.length // => 12972
// secrets.filter(_.length != 5) // => Nil

//(2)
def removeN[A](l: List[A], x: A, n: Int) : List[A] = {
  val identified = l.filter(_ == x).take(n)
  l.diff(identified)
}



// removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
// removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
// removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
// removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

// (3)
abstract class Tip
case object Absent extends Tip
case object Present extends Tip
case object Correct extends Tip



def pool(secret: String, word: String): List[Char] = {
  secret.zip(word.toList).filter{ case (c1, c2) => c1 != c2 }.map(_._1).toList
}

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = {
  if (word.isEmpty) Nil
  else if (word.head == secret.head) Correct :: aux(secret.tail, word.tail, pool)
  else if (pool.contains(word.head)) Present :: aux(secret.tail, word.tail, removeN(pool, word.head, 1))
  else Absent :: aux(secret.tail, word.tail, pool)
}

def score(secret: String, word: String) : List[Tip] = {
  aux(secret.toList, word.toList, pool(secret, word))
}


// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int = t match {
  case Correct => 10
  case Present => 1
  case Absent => 0
  case _ => 0
}

def iscore(secret: String, word: String) : Int = {
  score(secret, word).map(eval).sum
}

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = {
  if (secrets.isEmpty) acc
  else {
    val score = iscore(secrets.head, word)
    if (score < current) lowest(secrets.tail, word, score, List(secrets.head))
    else if (score == current) lowest(secrets.tail, word, current, secrets.head :: acc)
    else lowest(secrets.tail, word, current, acc)
  }
}

def evil(secrets: List[String], word: String) = {
  lowest(secrets, word, Int.MaxValue, Nil)
}


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] = {
  val chars = secrets.flatten
  val chars_distinct = chars.distinct
  chars_distinct.map(c => (c, 1 - (chars.count(_ == c).toDouble / chars.length))).toMap
}

// (7)
def rank(frqs: Map[Char, Double], s: String) = {
  s.map(c => frqs(c)).sum
}

def ranked_evil(secrets: List[String], word: String) = {
  val freq = frequencies(secrets)
  val evil_out = evil(secrets, word)
  val ranked = evil_out.map(s => (s, rank(freq, s)))
  val res = ranked.sortBy(_._2).map(_._1).last
  List(res)
}


}






// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
