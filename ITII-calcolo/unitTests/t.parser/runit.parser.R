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
	result <- create_randomVariable(name="Z",lag=0,power=0)
	checkEquals(parser(stringRandomVariable),result)

	# test3: a simple randomVariable with lag
	stringRandomVariable <- "Z_{ t -4}"; class(stringRandomVariable) <- "stringRandomVariable"
	result <- create_randomVariable(name="Z",lag=4,power=0)
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
}


test.parse.stringMonomial <- function() {
	
	parser.stringMonomial <- function(x) {
		
		# x: a string monomial, i.e. "3*a^2*b^3*Z_t^3"
		x <- trim(x)
		if (x=="") stop("Error in parser.stringMonomial: the argument is an empty string.")
		result <- strsplit(x, split="\\*")[[1]]
		
		number <- 1
		symbols <- create_symbols()
		randoms <- create_randomVariables()
		
		for (i in result) {
			tmp <- i
			class(tmp) <- identifySymbolComponent(i)
			tmp <- parser(tmp)
			
			if (class(tmp)=="randomVariable") {
				randoms[[length(randoms)+1]] <- tmp
			} else {
				if (class(tmp)=="symbol") {
					symbols[[length(symbols)+1]] <- tmp
				} else {
					number <- number * tmp
				}
			}		
		}
		
		return(create_monomial(number,symbols=symbols,randoms=randoms))
	}
	
	# test 1
	x <- "1"; class(x)<-"stringMonomial"
	parser(x)
	
	# da continuare qui
	checkEquals(FALSE,TRUE)
}
