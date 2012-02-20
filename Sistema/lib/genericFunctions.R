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

# union is a generic function used to merge Positions or Portfolios
setGeneric("union",function(x,y,...) standardGeneric("union"))
