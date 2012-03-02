# TODO: Add comment
# 
# Author: ortellic
###############################################################################

setClass("Symbols",contains="list")

setMethod("sort","Symbols",function(x) {
			if (length(x)==0) return(x)
			
			names <- sapply(x,function(x) return(x@name))
			order <- order(names)
			result <- new("Symbols",x[order])
			return(result)
		}
)

setMethod("*", signature(e1="Symbols",e2="Symbols"), 
		function(e1,e2) {
			
			if (length(e2)==0) return(e1)
			if (length(e1)==0) return(e2)
			
			tmp <- list()
			for(sym_b in b) {
				copy <- TRUE
				for (i in 1:length(a)) {
					if (sym_b@name==a[[i]]@name) {
						a[[i]]@power=a[[i]]@power+sym_b@power
						copy <- FALSE
						break
					}
				}
				if (copy) tmp[[length(tmp)+1]] <- sym_b
			}
			
			if (length(tmp)>0) { a <- new("Symbols",c(a,tmp))}
			a <- sort(a)
			return(a)
		}
)


setMethod("compact","Symbols",
		function(a) {
			# a symbol
			
			la <- length(a)
			if (la <= 1) return(a)
			
			tmp <- list()
			tmp[[1]] <- a[[1]]
			for(symbol in a[-1]) {
				copy <- TRUE
				for (i in 1:length(tmp)) {
					if(symbol@name==tmp[[i]]@name) {
						tmp[[i]]@power <- tmp[[i]]@power + symbol@power
						copy <- FALSE
						break
					}
				}
				if (copy) tmp[[i+1]] <- symbol
			}
			
			return(new("Symbols",tmp))
		}
)

setMethod("==",signature(e1="Symbols",e2="Symbols"),
		function(e1,e2) {
			return(identical(e1,e2))
		}
)


setMethod("toString","Symbols",
		function(x){			
			result <- sapply(x,toString)
			result <- paste(result,collapse="*")
			return(result)
		}
)
