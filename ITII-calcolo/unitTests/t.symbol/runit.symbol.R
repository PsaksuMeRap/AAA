# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.create_symbol <- function() {
	
	# randomVariable <- create_monomio(name="")
	
	symbol <- create_symbol()
			
	checkEquals(symbol$name,"")
	checkEquals(symbol$power,1)
	checkEquals(class(symbol),"symbol")
	
	symbol <- create_symbol("pippo",12)
	
	checkEquals(symbol$name,"pippo")
	checkEquals(symbol$power,12)

}

test.create_symbols <- function() {
	
	symbol <- create_symbol("a")
	symbols <- create_symbols(symbol)
	
	checkEquals(symbols[[1]]$name,"a")
	checkEquals(symbols[[1]]$power,1)
	checkEquals(class(symbols),"symbols")
	
	# check when passing no symbol
	symbols <- create_symbols()
	
	checkEquals(class(symbols),"symbols")
	
}

test.multiply_two_symbols <- function() {
	
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	
	c <- symbol1 * symbol2
	checkEquals(class(c),"symbols")
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[2]]$name,"b")
	
	# check when same symbol
	symbol2 <- create_symbol("a",2)
	
	c <- symbol1 * symbol2
	checkEquals(class(c),"symbols")
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[1]]$power,6)	
}


test.sort_symbols <- function() {
	
}