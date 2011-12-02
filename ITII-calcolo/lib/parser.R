# TODO: Add comment
# 
# Author: ortellic
###############################################################################


parser <- function(x) UseMethod("parser",x)

parser.stringSymbol <- function(x) {
	# x: a symbol string, i.e. "a^3" o "bdkdk"
	x <- trim(x)
	if (x=="") stop("Error in parser.stringSymbol: the argument is an empty string.")
	
	result <- strsplit(x, split="\\^")[[1]]
	if (length(result)==1) {
		return(create_symbol(name=trim(result[1]))) 
	} else {
		return(create_symbol(name=trim(result[1]),power=as.numeric(result[2])))
	}
}


parser.number <- function(x) {
	# x: a numer, i.e. "323.5" o "12^3"
	x <- trim(x)
	if (x=="") stop("Error in parser.number: the argument is an empty string.")
	
	result <- strsplit(x, split="\\^")[[1]]
	if (length(result)==1) return(as.numeric(result[1])) else return(as.numeric(result[1])^as.numeric(result[2]))
}



parser.stringRandomVariable <- function(x) {
	# x: a random string, i.e. "Z_t^3" o "Z_{t-1}"
	x <- trim(x)
	if (x=="") stop("Error in parser.stringRandomVariable: the argument is an empty string.")
	
	# verifica la presenza di una potenza
	result <- strsplit(x, split="\\^")[[1]]
	
	if (length(result)==2) {
		power = trim(result[2])
		if (substr(power,1,1)=="-") {
			power <- -1.0*as.numeric(trim(substr(power,2,nchar(power)))) 
		} else {
			power <- as.numeric(power)
		}
	} else {
		power=1
	}
	result <- result[1]
	
	# determina il lag
	result <- strsplit(result, split="_")[[1]]
	name <- trim(result[[1]])
	lag  <- trim(result[[2]])
	# se il primo carattere non e' "{" allora deve essere "t"
	if (substr(lag,1,1)=="{") {
		# elimina "{}
		lag <- trim(substr(lag,2,nchar(lag)-1))
		# elimina la "t"
		lag <- trim(substr(lag,2,nchar(lag)))
		if (nchar(lag)==0) lag <- as.numeric(0)
		if (substr(lag,1,1)=="-") {
			lag <- as.numeric(trim(substr(lag,2,nchar(lag)))) 
		}
		if (substr(lag,1,1)=="+") {
			lag <- -1.0 * as.numeric(trim(substr(lag,2,nchar(lag))))
		}
	} else {
		lag <- (0)
	}
	
	return(create_randomVariable(name,lag,power))
}

identifySymbolComponent <- function(x) {
	# pure number or a symbol or a randomVariable
	
	if (grepl("_",x)) return("stringRandomVariable")
	if (grepl("[[:alpha:]]",x)) return("stringSymbol")
	return("number")
	
}


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
		
		if (is.element("randomVariable",class(tmp))) {
			randoms[[length(randoms)+1]] <- tmp
		} else {
			if (class(tmp)=="symbol") {
				symbols[[length(symbols)+1]] <- tmp
			} else {
				number <- number * tmp
			}
		}		
	}
	
	return(create_monomial(number,symbols=sort(symbols),randoms=sort(randoms)))
}

parser.stringMonomials <- function(x) {
	x <- trim(x)
	if (x=="") stop("Error in parser.stringMonomials: the argument is an empty string.")
	
	result <- as.list(strsplit(x, split="\\+")[[1]])
	result <- lapply(result,function(x) {class(x)<-"stringMonomial";return(x)})
	
	monomials <- lapply(result,parser)
	class(monomials) <- "monomials"	
	return(monomials)
}

create_stringMonomial <- function(string) {
	x <- as.character(string); class(x) <- "stringMonomial"
	return(x)
}

monomialFromString <- function(x) {
	return(parser(create_stringMonomial(x)))
}

create_stringMonomials <- function(string) {
	x <- as.character(string); class(x) <- "stringMonomials"
	return(x)
}

monomialsFromString <- function(x) {
	return(parser(create_stringMonomials(x)))
}
