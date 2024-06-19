// Main Part 1 about a really dumb investment strategy
//===================================================

object M1 {

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "HCP") 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


// (1) 
def get_january_data(symbol: String, year: Int) : List[String] = {
    val file = Source.fromFile(s"$symbol.csv")
    val data = for (line <- file.getLines() if line.startsWith(s"${year.toString}-01")) yield line
    data.toList
}


// (2) 
def get_first_price(symbol: String, year: Int) : Option[Double] = {
    val data = get_january_data(symbol, year)
    if (data.isEmpty) None
    else data.head.split(",")(1).toDoubleOption
}


// (3) 
def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = {
    val first_prices = for (yr <- years) yield (for (symbol <- portfolio) yield get_first_price(symbol, yr))
    first_prices.toList
}



// (4) 
def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = {
    if (price_old.isDefined && price_new.isDefined) Some((price_new.get - price_old.get) / price_old.get)
    else None
}



// (5) 
def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = {
    // all but first value since we can't compare to prev
    val data_without_first = data.tail
    data_without_first.map(year => {
        year.map(price => if (!price.isDefined) price else {
            // data(<prev-yr-index>)(<company index>)
            val price_old = data(data.indexOf(year)-1)(year.indexOf(price))
            get_delta(price_old, price)
        })
    })
}

// (6) 
def yearly_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    ((data(index).flatten.sum / data(index).flatten.size) * balance).toLong + balance
}


// (7) 
def compound_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    if (index >= data.size) balance
    else compound_yield(data, yearly_yield(data, balance, index), index+1)
}

def investment(portfolio: List[String], years: Range, start_balance: Long) : Long = {
    compound_yield(get_deltas(get_prices(portfolio, years)), start_balance, 0)
}




//Test cases for the two portfolios given above

// println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
// println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))


}




// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
