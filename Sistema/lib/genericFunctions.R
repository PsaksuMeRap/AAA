# TODO: Add comment
# 
# Author: claudio
###############################################################################

fun <- function(x) standardGeneric("toString")
setGeneric("toString", fun)

# print is still a generic function
fun <- function(x) standardGeneric("print")
setGeneric("print", fun)

fun <- function(x,divisor) standardGeneric("divide")
setGeneric("divide", fun)

# join is a generic function used to join Positions or Portfolios
# the positions are joined and duplicated positions are not removed
# or summed together
setGeneric("join",function(x,y,...) standardGeneric("join"))

removeStartEndSpaces <- function(string) {
	result <- sub("^[[:blank:]]+", "", string)
	result <- sub("[[:blank:]]+$", "", result)
	return(result)
}
