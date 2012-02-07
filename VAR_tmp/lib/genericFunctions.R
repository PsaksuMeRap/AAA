# TODO: Add comment
# 
# Author: claudio
###############################################################################

fun <- function(x) standardGeneric("toString")
setGeneric("toString", fun)

fun <- function(x) standardGeneric("print")
setGeneric("print", fun)

fun <- function(x,divisor) standardGeneric("divide")
setGeneric("divide", fun)

