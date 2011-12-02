# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.create_symbol <- function() {
	
	# randomVariable <- create_monomio(name="")
	
	symbol <- create_symbol()
			
	checkEquals(symbol$name,"a")
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

test.multiply_two_symbol <- function() {
	
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	
	# multiply two different symbols
	c <- symbol1 * symbol2
	checkEquals(class(c),"symbols")
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[2]]$name,"b")
	checkEquals(c[[1]]$power,4)
	checkEquals(c[[2]]$power,2)
	
	# check that symbols are automatically ordered
	c <- symbol2 * symbol1
	checkEquals(class(c),"symbols")
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[2]]$name,"b")
	checkEquals(c[[1]]$power,4)
	checkEquals(c[[2]]$power,2)
	
	
	
	# check when same symbol
	symbol2 <- create_symbol("a",2)
	
	c <- symbol1 * symbol2
	checkEquals(class(c),"symbols")
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[1]]$power,6)	
	
	# check error
	checkException(symbol1 * 2)
}

test.multiply_two_symbols <- function() {
	
	# check symbols * empty symbols
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	symbols1 <- symbol1 * symbol2
	symbols2 <- create_symbols()

	c <- symbols1 * symbols2
	
	checkEquals(class(c),"symbols")
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[1]]$power,4)
	checkEquals(c[[2]]$name,"b")
	checkEquals(c[[2]]$power,2)
	checkEquals(length(c),2)
	
	# check commutativity (symbols are sorted!)
	c <- symbols2 * symbols1
	checkEquals(class(c),"symbols")
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[1]]$power,4)
	checkEquals(c[[2]]$name,"b")
	checkEquals(c[[2]]$power,2)
	checkEquals(length(c),2)
	
	# multiply two symbols of different length
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	symbols1 <- symbol1 * symbol2
	symbols2 <- create_symbol("d",2) * create_symbol("e",1) * create_symbols(create_symbol("c",1))

	c <- symbols1 * symbols2
	checkEquals(class(c),"symbols")
	checkEquals(length(c),5)
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[1]]$power,4)
	checkEquals(c[[2]]$name,"b")
	checkEquals(c[[2]]$power,2)
	checkEquals(c[[3]]$name,"c")
	checkEquals(c[[3]]$power,1)
	checkEquals(c[[4]]$name,"d")
	checkEquals(c[[4]]$power,2)
	checkEquals(c[[5]]$name,"e")
	checkEquals(c[[5]]$power,1)

	
	# square one symbols
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	tmp <- symbol1 * symbol2
	c <- tmp * tmp
	checkEquals(class(c),"symbols")
	checkEquals(length(c),2)
	checkEquals(c[[1]]$name,"a")
	checkEquals(c[[1]]$power,8)
	checkEquals(c[[2]]$name,"b")
	checkEquals(c[[2]]$power,4)

	# error
	checkException(tmp * 2)
}

test.equality_two_symbol <- function() {
	
	# are equals
	a <- create_symbol("a",4)
	b <- create_symbol("a",4)
	checkEquals(a==b,TRUE)
	
	# are different names
	a <- create_symbol("a",4)
	b <- create_symbol("b",4)
	checkEquals(a==b,FALSE)
	
	# are different powers
	a <- create_symbol("a",4)
	b <- create_symbol("a",5)
	checkEquals(a==b,FALSE)
}

test.equality_two_symbols <- function() {
	
	# compare same symbols 
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	a <- symbol1 * symbol2
	b <- symbol1 * symbol2
		
	checkEquals(a==b,TRUE)
	
	# compare same symbols but in different order 
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	a <- symbol1 * symbol2
	b <- symbol2 * symbol1
	
	checkEquals(a==b,TRUE)
	
	# compare symbols of different length
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	a <- symbol1 * symbol2
	b <- create_symbol("d",2) * create_symbol("e",1) * create_symbols(create_symbol("c",1))
	
	checkEquals(a==b,FALSE)
	
	# compare different symbols of same length
	symbol1 <- create_symbol("a",4)
	symbol2 <- create_symbol("b",2)
	a <- symbol1 * symbol2
	b <- create_symbol("d",2) * create_symbol("e",1) 
	
	checkEquals(a==b,FALSE)
	
}


test.sort_symbols <- function() {
	
	# check when null symbols
	symbols <- create_symbols()
	
	a <- sort(symbols)
	checkEquals(class(a),"symbols")
	checkEquals(length(a),0)
	
	# check a one lenth symbols list 
	symbols <- create_symbols(create_symbol("b",4))
	a <- sort(symbols)
	checkEquals(class(a),"symbols")
	checkEquals(length(a),1)
	checkEquals(a[[1]]$name,"b")
	checkEquals(a[[1]]$power,4)
	
	# check a lenth 3 symbols list 
	symbol1 <- create_symbol("b",4)
	symbol2 <- create_symbol("a",2)
	symbol3 <- create_symbol("c",1)
	
	symbols <- symbol1 * symbol2 * create_symbols(symbol3)
	a <- sort(symbols)
	checkEquals(class(a),"symbols")
	checkEquals(length(a),3)
	checkEquals(a[[1]]$name,"a")
	checkEquals(a[[1]]$power,2)
	
	checkEquals(a[[2]]$name,"b")
	checkEquals(a[[2]]$power,4)
	
	checkEquals(a[[3]]$name,"c")
	checkEquals(a[[3]]$power,1)
	
}

test.toString.symbol <- function() {
	a5 <- create_symbol("a",5)
	checkEquals(toString(a5),"a^5")
	
	# empty symbol
	a <- create_symbol()
	checkEquals(toString(a),"a")
	
	a5 <- create_symbol("a",5)
	b4 <- create_symbol("b",4)
	product <- a5 * b4	
	
	checkEquals(toString(product),"a^5*b^4")
	
	# a empty symbols
	a <- create_symbols()
	checkEquals(toString(a),"")
}


