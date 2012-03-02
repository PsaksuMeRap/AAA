# TODO: Add comment
# 
# Author: ortellic
###############################################################################

setClass("Symbol",representation(name="character",power="integer"))

create_symbol <- function(name="a",power=1) {
	symbol <- list()
	class(symbol) <- "symbol"
	
	symbol$name <- name
	symbol$power <- as.integer(power)
	return(symbol)
}


setMethod("*",signature(e1="Symbol",e2="Symbol"), function(e1,e2) {			
			if (e1@name==e2@name) {
				e1@power=as.integer(e1@power+e2@power)
				return(e1)
			}	
			c <- new("Symbols",list(a))
			c[[2]] <- b
			return(sort(c))
		}
)


setMethod("==",signature(e1="Symbol",e2="Symbol"), function(e1,e2) {
			return (identical(e1,e2))
		}
)


setGeneric("toString", function(x) standardGeneric("toString") )

setMethod("toString", "Symbol", function(x) {
			if (x@power==1) return(x@name)
			return(paste(x@name,"^",x@power,sep=""))
		}
)

