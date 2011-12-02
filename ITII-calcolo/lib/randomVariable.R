# TODO: Add comment
# 
# Author: ortellic
###############################################################################


create_randomVariable <- function(name="epsilon",lag=0,power=1) {
	randomVariable <- create_symbol(name=name,power=as.numeric(power))
	randomVariable$lag <- as.numeric(lag)
	
	
	class(randomVariable) <- c("randomVariable",class(randomVariable))
	return(randomVariable)
}

"==.randomVariable" <- function(a,b) {
	return(identical(a,b))
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
	return(sort(c))
	
	stop("Error in function '*.randomVariable': entered randomVariable are not valid randomVariables")
}


toString.randomVariable <- function(x) {
	
	if (x$lag==0) str <- "" else if (x$lag>0) str <- paste("-",x$lag,sep="") else str <- paste("+",abs(x$lag),sep="")
	if (x$power!=1) result <- paste(x$name,"_{t",str,"}^",x$power,sep="") else result <- paste(x$name,"_{t",str,"}",sep="")
	
	return(result)
}

