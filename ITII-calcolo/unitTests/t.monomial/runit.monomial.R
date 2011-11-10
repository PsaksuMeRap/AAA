# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.create_monomial <- function() {
	
	# create empty monomial
	monomial <- create_monomial()
	
	checkEquals(class(monomial),"monomial")
	checkEquals(monomial$number,1)
	checkEquals(class(monomial$symbols),"symbols")
	checkEquals(class(monomial$randoms),"randomVariables")
	checkEquals(length(monomial$symbols),0)
	checkEquals(length(monomial$randoms),0)

	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	monomial <- create_monomial(2,symbols,randomVariables)
	checkEquals(monomial$number,2)
	checkEquals(class(monomial$symbols),"symbols")
	checkEquals(class(monomial$randoms),"randomVariables")
	checkEquals(length(monomial$symbols),2)
	checkEquals(length(monomial$randoms),1)
}


test.sum_two_monomial <- function() {
	
	# sum two empty monomial
	monomial <- create_monomial()
	
	a <- monomial + monomial
	checkEquals(class(a),"monomials")
	checkEquals(length(a),1)	
	checkEquals(a[[1]]$number,2)

	
	# sum two different monomial
	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols,randomVariables)
	# create 2*c^3
	symbols <- create_symbols(create_symbol(name="c",power=3))
	randomVariables <- create_randomVariables()
	b <- create_monomial(4,symbols,randomVariables)
	
	c <- a + b 
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),2)	
	checkEquals(c[[1]],a)
	checkEquals(c[[2]],b)
	
}


test.sum_two_monomials <- function() {	
	
	# sum three empty monomial
	monomial <- create_monomial()
	
	a <- monomial + monomial + create_monomials(monomial)
	checkEquals(class(a),"monomials")
	checkEquals(length(a),1)	
	checkEquals(a[[1]]$number,3)
	
	
	# sum three different monomials
	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols,randomVariables)
	# create 2*c^3
	symbols <- create_symbols(create_symbol(name="c",power=3))
	randomVariables <- create_randomVariables()
	b <- create_monomial(4,symbols,randomVariables)
	
	c <- create_monomials(a) + create_monomials(b) + create_monomials(monomial)
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),3)	
	checkEquals(c[[1]],a)
	checkEquals(c[[2]],b)
	checkEquals(c[[3]],monomial)
	
	# sum two identical monomials
	# create 2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols,randomVariables)
	
	c <- create_monomials(a) + create_monomials(a)
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),1)	
	checkEquals(c[[1]]$number,4)
	checkEquals(c[[1]]$symbols,symbols)
	checkEquals(c[[1]]$randoms,randomVariables)
}


test.multiply_two_monomial <- function() {
	
	
	# multiply two empty monomial
	monomial <- create_monomial()
	
	a <- monomial * monomial
	checkEquals(class(a),"monomials")
	checkEquals(length(a),1)	
	checkEquals(a[[1]]$number,1)
	
	
	# multiply two different monomial
	# create 2*a^2*b^3Z_t^3
	symbols1 <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables1 <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols1,randomVariables1)
	# create 2*c^3
	symbols2 <- create_symbols(create_symbol(name="c",power=3))
	randomVariables2 <- create_randomVariables()
	b <- create_monomial(4,symbols2,randomVariables2)
	
	c <- a * b 
	
	checkEquals(class(c),"monomials")
	checkEquals(length(c),1)	
	checkEquals(c[[1]]$number,8)
	checkEquals(c[[1]]$symbols,symbols1*symbols2)
	checkEquals(c[[1]]$randoms,randomVariables1*randomVariables2)
}


test.compact.monomials <- function() {
	# compact an empty monomial
	a <- list()
	class(a) <- "monomials"
	
	b <- compact(a)
	checkEquals(class(b),"monomials")
	checkEquals(length(b),0)	
	
	# compact a monomials of length 1
	a <- create_monomials()
	
	b <- compact(a)
	checkEquals(class(b),"monomials")
	checkEquals(length(b),1)	
	checkEquals(b[[1]]$number,1)
	
	# compact three unit monomials
	monomial <- create_monomial()
	
	a <- list(monomial,monomial,monomial)
	class(a) <- "monomials"
	
	b <- compact(a)
	checkEquals(class(b),"monomials")
	checkEquals(length(b),1)	
	checkEquals(b[[1]]$number,3)
	
}

test.multiply_two_monomials <- function() {
	
	# multiply two unit monomials
	a <- create_monomials()
	b <- a * a
	checkEquals(class(b),"monomials")
	checkEquals(length(b),1)	
	checkEquals(b[[1]]$number,1)
	
	# multiply two different monomials
	# create 2*a^2*b^3Z_t^3
	symbols1 <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables1 <- create_randomVariables(create_randomVariable("Z",0,3))
	a <- create_monomial(2,symbols1,randomVariables1)
	# create 2*c^3
	symbols2 <- create_symbols(create_symbol(name="c",power=3))
	randomVariables2 <- create_randomVariables()
	b <- create_monomial(4,symbols2,randomVariables2)
	
	# create a*b^3W_t^2
	symbols3 <- create_symbol(name="a") * create_symbol(name="b",power=3)
	randomVariables3 <- create_randomVariables(create_randomVariable("W",0,2))
	c <- create_monomial(1,symbols3,randomVariables3)
	# create 8*c^4
	symbols4 <- create_symbols(create_symbol(name="c",power=4))
	randomVariables4 <- create_randomVariables()
	d <- create_monomial(8,symbols4,randomVariables4)
	
	# create 2*f^0.5
	symbols5 <- create_symbols(create_symbol(name="f",power=0.5))
	randomVariables5 <- create_randomVariables()
	e <- create_monomial(2,symbols5,randomVariables5)
	
	a1 <- a + b
	a2 <- c + d + create_monomials(e)
	
	b <- a1 * a2
	checkEquals(class(b),"monomials")
	checkEquals(length(b),6)	
	checkEquals(b[[1]]$number,2)
	checkEquals(b[[1]]$random,randomVariables1*randomVariables3)
	xxx <- list(); class(xxx) <- "randomVariables"
	checkEquals(b[[6]]$random,xxx)
}



test.create_monomials <- function() {
	
	# create monomials with unity as the only monomial
	monomials <- create_monomials()

	checkEquals(class(monomials),"monomials")
	checkEquals(monomials[[1]],create_monomial())
	
	# create monomials with  2*a^2*b^3Z_t^3
	symbols <- create_symbol(name="a",power=2) * create_symbol(name="b",power=3)
	randomVariables <- create_randomVariables(create_randomVariable("Z",0,3))
	monomial <- create_monomial(2,symbols,randomVariables)
	
	monomials <- create_monomials(monomial)
	
	checkEquals(class(monomials),"monomials")
	checkEquals(monomials[[1]],monomial)
}



test.toString.monomial <- function() {
	
	# create 2*a^2*b^3*Z_t^3
	monomial <- constructMonomial()[[1]]
	checkEquals(toString(monomial),"2*a^2*b^3*Z_{t}^3")
	
	# create monomial with empty symbols
	monomial <- create_monomial(randoms=create_randomVariables(create_randomVariable(lag=2,power=3)))
	checkEquals(toString(monomial),"1*epsilon_{t-2}^3")
	
	# create empty monomial
	monomial <- create_monomial()
	checkEquals(toString(monomial),"1")
}


test.toString.monomials <- function() {
	
	toString.monomials <- function(monomials) {
		result <- sapply(monomials,toString)
		
		if (length(result)>1) {
			sign <- sapply(monomials,function(x){if(x$number>=0) return(" + ") else return(" - ")})
			first <- result[1]
			result <- sapply(result,function(x){return(sub('^-',"",x))})
			result <- paste(sign[-1],result[-1],sep="",collapse="")
			result = paste(first,result,sep="")
		}
		
		return(result)
	}
	
	# empty monomial (only "1")
	a <- create_monomials()
	checkEquals(toString(a),"1")
	
	# create 2*a^2*b^3*Z_t^3 + 4*c^3
	monomial1 <- constructMonomial()[[1]]
	monomial2 <- constructMonomial()[[2]]
	a <- monomial1 + monomial2
	checkEquals(toString(a),"2*a^2*b^3*Z_{t}^3 + 4*c^3")
	
	# create 2*a^2*b^3*Z_t^3 - 4*c^3
	monomial1 <- constructMonomial()[[1]]
	monomial2 <- constructMonomial()[[3]]
	a <- monomial1 + monomial2
	checkEquals(toString(a),"2*a^2*b^3*Z_{t}^3 - 4*c^3")
	
	# create 2*a^2*b^3*Z_t^3 - 4*c^3 +  4*c^3*epsilon_t^1
	monomial1 <- constructMonomial()[[1]]
	monomial2 <- constructMonomial()[[3]]
	monomial3 <- constructMonomial()[[2]]
	monomial3$randoms <- create_randomVariables(create_randomVariable())
	
	a <- monomial1 + monomial2 + create_monomials(monomial3)
	checkEquals(toString(a),"2*a^2*b^3*Z_{t}^3 - 4*c^3 + 4*c^3*epsilon_{t}^1")	
}
