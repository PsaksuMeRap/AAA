# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.parser.stringSymbol <- function() {
	
	# test1: an empty symbol
	stringSymbol <- ""; class(stringSymbol) <- "stringSymbol"
	checkException(parser(stringSymbol))
	
	# test2: a simple symbol
	stringSymbol <- "aiuto"; class(stringSymbol) <- "stringSymbol"
	checkEquals(parser(stringSymbol),create_symbol("aiuto"))
	
	# test3: a symbol with power
	stringSymbol <- "aiuto^4"; class(stringSymbol) <- "stringSymbol"
	checkEquals(parser(stringSymbol),create_symbol("aiuto",4))
	
	# test4: a symbol with power and spaces
	stringSymbol <- " aiuto ^ 4 "; class(stringSymbol) <- "stringSymbol"
	checkEquals(parser(stringSymbol),create_symbol("aiuto",4))
}



test.parser.number <- function() {

	# test1: an empty number
	number <- ""; class(number) <- "number"
	checkException(parser(number))
	
	# test2: a simple symbol
	number <- "15"; class(number) <- "number"
	checkEquals(parser(number),15)
	
	# test3: a symbol with power
	number <- "15^2"; class(number) <- "number"
	checkEquals(parser(number),225)
	
	# test3: a symbol with power and spaces
	number <- " 15 ^ 2 "; class(number) <- "number"
	checkEquals(parser(number),225)
	
}

test.parser.stringRandomVariable <- function() {
	
	# test1: an empty randomVariable
	stringRandomVariable <- ""; class(stringRandomVariable) <- "stringRandomVariable"
	checkException(parser(stringRandomVariable))
	
	# test2: a simple randomVariable
	stringRandomVariable <- "Z_t"; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=0,power=1)
	checkEquals(parser(stringRandomVariable),result)

	# test3: a simple randomVariable with lag
	stringRandomVariable <- "Z_{ t -4}"; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=4,power=1)
	checkEquals(parser(stringRandomVariable),result)
	
	# test4: a randomVariable with power
	stringRandomVariable <- "Z_t^2"; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=0,power=2)
	checkEquals(parser(stringRandomVariable),result)
	
	# test5: a randomVariable with lag and power
	stringRandomVariable <- "Z _ { t   -    5   }  ^ - 2  "; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=5,power=-2)
	checkEquals(parser(stringRandomVariable),result)
	
	# test6: a randomVariable with simple lag and power
	stringRandomVariable <- "Z _ { t     }  ^ - 2  "; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=0,power=-2)
	checkEquals(parser(stringRandomVariable),result)
	
	# test7: a randomVariable with negative lag and power
	stringRandomVariable <- "Z _ { t   +    5   }  ^ - 2  "; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=-5,power=-2)
	checkEquals(parser(stringRandomVariable),result)
	
	# test8: a randomVariable with no power
	stringRandomVariable <- "Z _ { t   +    5   }"; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=-5)
	checkEquals(parser(stringRandomVariable),result)
	
}


test.identifySymbolComponent <- function() {
	
	# test 1
	x <- "Z_{t-3}^23"
	checkEquals(identifySymbolComponent(x),"stringRandomVariable")
	# test 2
	x <- "adf55df^3"
	checkEquals(identifySymbolComponent(x),"stringSymbol")
	# test 3
	x <- "5^3"
	checkEquals(identifySymbolComponent(x),"number")
	# test 4
	x <- "x^3"
	checkEquals(identifySymbolComponent(x),"stringSymbol")
}


test.parse.stringMonomial <- function() {
	
	# test 1
	x <- "1"; class(x)<-"stringMonomial"
	checkEquals(parser(x),create_monomial(1))
	
	# test 2
	x <- "x^23"; class(x)<-"stringMonomial"
	checkEquals(parser(x),create_monomial(1,symbols=create_symbols(create_symbol("x",power=23))))
	
	# test 3
	x <- "x_{t-4}^23"; class(x)<-"stringMonomial"
	checkEquals(parser(x),create_monomial(1,randoms=create_randomVariables(create_randomVariable("x",lag=4,power=23))))
	
	# da continuare qui
	x <- "2*y_t^2*z_{t-5}^12*a*y^2*12"; class(x) <- "stringMonomial"
	randoms <- create_randomVariable("y",power=2) * create_randomVariable("z",lag=5,power=12)
	symbols <- create_symbol("a") * create_symbol("y",power=2)
	checkEquals(parser(x),create_monomial(24,symbols,randoms))

}
