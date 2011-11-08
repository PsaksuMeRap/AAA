# TODO: Add comment
# 
# Author: ortellic
###############################################################################


create_randomVariable <- function(name="epsilon",lag=0,power=1) {
	randomVariable <- create_symbol(name=name,power=power)
	randomVariable$lag = lag
	
	
	class(randomVariable) <- c("randomVariable",class(randomVariable))
	return(randomVariable)
}

"==.randomVariable" <- function(a,b) {
	if (a$name != b$name) return(FALSE)
	if (a$lag != b$lag) return(FALSE)		
	if (a$power != b$power) return(FALSE)
	
	return(TRUE)	
}


"*.randomVariable" <- function(a,b) {
	
	if (a$name==b$name & a$lag==b$lag) {
		a$power=a$power+b$power
		return(create_randomVariables(a))
	}
	c <- create_randomVariables(a)
	c[[2]] <- b
	return(c)
	
	stop("Error in function '*.randomVariable': entered randomVariable are not valid randomVariables")
}
