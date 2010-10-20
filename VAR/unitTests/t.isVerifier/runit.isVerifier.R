# TODO: Add comment
# 
# Author: claudio
###############################################################################

# foo <- function(x) UseMethod("foo",x)
# foo.pippo <- function(x) {print("ecco pippo")}

test.isInCurrency <- function() {
	source("./lib/position.R")
	
	isInCurrency <- function(x,currency) useMethod("isInCurrency",x)
	isInCurrency.default <- function(x,currency) {
		stop(paste("Error: isInCurrency does not support a",
						"method for objects of class",class(x))
		)
	}
	
	isInCurrency.position <- function(x,currency) {
		return(x$currency==currency)
	}
	isInCurrency.positions <- functions(x,currency) {
		result <- lapply(x$positions,isInCurrency,currency)
		return(result)
	}
	
	# crea la posizione
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	checkEquals(isInCurrency(position,"USD"))
}



