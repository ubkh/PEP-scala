// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {

// ADD YOUR CODE BELOW
//======================


//(1) 
def collatz(n: Long) : Long = {
    // base
    if (n == 1) 0 else
        // recursive
        if (n % 2 != 0) 1 + collatz((3*n)+1) else 1 + collatz(n/2)
}


//(2) 
def collatz_max(bnd: Long) : (Long, Long) = {
    val steps = for (a <- (1L to bnd).toList) yield collatz(a)
    (steps.max, steps.indexOf(steps.max)+1)
}

//(3)
def is_pow_of_two(n: Long) : Boolean = (n & n-1) == 0

def is_hard(n: Long) : Boolean = is_pow_of_two((3*n)+1)

def last_odd(n: Long) : Long = {
    if (!is_pow_of_two(n) && is_hard(n)) n else
        if (n % 2 != 0) last_odd((3*n)+1) else last_odd(n/2)
}

}



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
