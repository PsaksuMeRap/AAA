# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("FactorString",contains="character")

setMethod("split",
		signature(x = "FactorString"),
		function (x, f, drop = FALSE, ...) {
			# factorString: a string like "instrument:equity,bond" or
			# "amount:<= 50 USD" or the negation form "instrument!: equity, bond"
			
			result <- unlist(strsplit(x,":"))
			
			if (length(result)<2) stop(paste("Error: splitFactorString should return 2 blocks.",x))
			result <- removeStartEndSpaces(result)
			
			# check if negation is required
			factor <- result[1]
			nbChar <- nchar(factor)
			if (substr(factor,nbChar,nbChar)=="!") {
				factor <- removeStartEndSpaces(substr(factor,1,nbChar-1))
				negation = TRUE
			} else {
				negation = FALSE
			}
			rm(nbChar)
			
			if (identical(factor,"amount")) {
				values <- new("ConstraintString",result[2])
			} else {
				# values <- removeStartEndSpaces(unlist(strsplit(result[2],",")))
				values <- result[[2]]
			}
			
			return(factorStringParsed=new("FactorStringParsed",criterium=factor,
							negation=negation,values=values))
		}
)
