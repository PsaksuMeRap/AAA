# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_randomVariable <- function() {
	
	randomVariable <- create_randomVariable(name="")
	
	checkEquals(randomVariable$name,"")
	checkEquals(randomVariable$lag,0)
	checkEquals(randomVariable$power,1)	
	checkEquals(class(randomVariable),c("randomVariable","symbol"))

}

test.create_randomVariables <- function() {
	
	rv1 <- create_randomVariable(name="a")
	randomVariables <- create_randomVariables(rv1)
	
	checkEquals(class(randomVariables),"randomVariables")
	checkEquals(length(randomVariables),1)
	checkEquals(class(randomVariables[[1]]),c("randomVariable","symbol"))
	
	# check when passing no random variables
	randomVariables <- create_randomVariables()
	
	checkEquals(class(randomVariables),"randomVariables")
	checkEquals(length(randomVariables),0)
}


test.multiply_two_randomVariable <- function() {
	
	# check same random variable
	rv1 <- create_randomVariable(name="a",lag=2)
	rv2 <- create_randomVariable(name="a",lag=2)
	
	a <- rv1 * rv2
	
	checkEquals(class(a),"randomVariables")
	checkEquals(length(a),1)
	checkEquals(a[[1]]$lag,2)
	checkEquals(a[[1]]$power,2)
	
	# check different random variables
	rv1 <- create_randomVariable(name="a",lag=2)
	rv2 <- create_randomVariable(name="b",lag=3,power=12)
	
	a <- rv1 * rv2
	
	checkEquals(class(a),"randomVariables")
	checkEquals(length(a),2)
	checkEquals(a[[1]]$lag,2)
	checkEquals(a[[2]]$power,12)
	
	# check that the rv are sorted
	rv1 <- create_randomVariable(name="a",lag=2)
	rv2 <- create_randomVariable(name="b",lag=3,power=12)
	
	a <- rv2 * rv1
	
	checkEquals(class(a),"randomVariables")
	checkEquals(length(a),2)
	checkEquals(a[[1]]$lag,2)
	checkEquals(a[[1]]$name,"a")
	checkEquals(a[[2]]$power,12)
	checkEquals(a[[2]]$name,"b")
	
	# check error
	checkException(a * 6)
}

test.multiply_two_randomVariables <- function() {
	
	# check same random variable
	rv1 <- create_randomVariable(name="a",lag=2)
	rv2 <- create_randomVariable(name="a",lag=2)
	
	a <- rv1 * rv2
	b <- a * a
	
	checkEquals(class(b),"randomVariables")
	checkEquals(length(a),1)
	checkEquals(b[[1]]$lag,2)
	checkEquals(b[[1]]$power,4)
	
	# check different random variables
	rv1 <- create_randomVariable(name="a",lag=2)
	rv2 <- create_randomVariable(name="b",lag=3,power=12)
	rv3 <- create_randomVariable(name="c",lag=2)
	rv4 <- create_randomVariable(name="b",lag=3,power=1)
	rv5 <- create_randomVariable(name="d")
	
	a <- rv1 * rv2
	b <- rv3 * rv4 * create_randomVariables(rv5)
	
	c <- a * b
	checkEquals(class(c),"randomVariables")
	checkEquals(length(c),4)
	checkEquals(c[[1]],rv1)
	checkEquals(c[[2]],create_randomVariable(name="b",lag=3,power=13))
	checkEquals(c[[3]],rv3)
	checkEquals(c[[4]],rv5)
	
	# check error
	checkException(c * 6)
}

test.sort_randomVariables <- function() {
	
	rv1 <- create_randomVariable(name="a",lag=2)
	rv2 <- create_randomVariable(name="b",lag=3)
	rv3 <- create_randomVariable(name="a",lag=1)
	randomVariables <- create_randomVariables(rv1)
	randomVariables[[2]] <- rv2
	randomVariables[[3]] <- rv3
	
	randomVariables <- sort(randomVariables)
	checkEquals(class(randomVariables),"randomVariables")
	checkEquals(length(randomVariables),3)
	checkEquals(randomVariables[[1]],rv3)
	checkEquals(randomVariables[[2]],rv1)
	checkEquals(randomVariables[[3]],rv2)
	
	# check when passing no random variables
	randomVariables <- sort(create_randomVariables())
	
	checkEquals(class(randomVariables),"randomVariables")
	checkEquals(length(randomVariables),0)
}



test.equality_two_randomVariable <- function() {
	
	# same RV
	a <- create_randomVariable(name="z")
	b <- create_randomVariable(name="z")
	
	checkEquals(a==b,TRUE)
	
	# different name
	a <- create_randomVariable(name="a")
	b <- create_randomVariable(name="z")
	
	checkEquals(a==b,FALSE)
	
	# different lag
	a <- create_randomVariable(name="a",lag=2)
	b <- create_randomVariable(name="a")
	
	checkEquals(a==b,FALSE)
	
	# different power
	a <- create_randomVariable(name="a",power=3)
	b <- create_randomVariable(name="a")
	
	checkEquals(a==b,FALSE)	
	
}


test.equality_two_randomVariables <- function() {
	
	# compare same randomVariables
	a <- create_randomVariables(create_randomVariable(name="z"))
	b <- create_randomVariables(create_randomVariable(name="z"))
	
	checkEquals(a==b,TRUE)
	
	# compare same randomVariables but in different order 
	a1 <- create_randomVariables(create_randomVariable(name="a"))
	a2 <- create_randomVariables(create_randomVariable(name="b"))
	a <- a1 * a2
	b <- a2 * a1
	
	checkEquals(a==b,TRUE)
	
	# compare randomVariables of different length
	a1 <- create_randomVariables(create_randomVariable(name="a"))
	a2 <- create_randomVariables(create_randomVariable(name="b"))
	a <- a1 * a2
	b <- create_randomVariable(name="b",lag=2) * create_randomVariable(name="a",lag=12,6) *
			create_randomVariables(create_randomVariable(name="a",2,3))
	
	checkEquals(a==b,FALSE)
	
	# compare different randomVariables of same length
	a1 <- create_randomVariables(create_randomVariable(name="a"))
	a2 <- create_randomVariables(create_randomVariable(name="b"))
	a <- a1 * a2
	b <- create_randomVariable(name="b",lag=2) * create_randomVariable(name="a",lag=12,6)
	
	
	checkEquals(a==b,FALSE)
	
}


test.toString.randomVariable <- function() {
	
	
	a <- create_randomVariable()
	
	checkEquals(toString(a),"epsilon_{t}")
	
	# con lag>0
	a <- create_randomVariable(lag=3,power=4)
	
	checkEquals(toString(a),"epsilon_{t-3}^4")
	
	# con lag<0
	a <- create_randomVariable(lag=-3,power=4)
	
	checkEquals(toString(a),"epsilon_{t+3}^4")
	
}


test.toString.randomVariables <- function() {
	
	a <- create_randomVariable(lag=1,power=1)
	b <- create_randomVariable("Z",1,2)
	c <- a * b
	
	result <- toString(c)
	checkEquals(toString(c),"epsilon_{t-1}*Z_{t-1}^2")
	
	# con empty randomVariables
	a <- create_randomVariables()
	checkEquals(toString(a),"")
	
}


test.disaggregateRandomVariable <- function() {
	
	disaggregate <- function(x) {
		
	}
	
	checkEquals(TRUE,FALSE)
}