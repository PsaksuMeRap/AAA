# TODO: Add comment
# 
# Author: claudio
###############################################################################



constraintFactory <- function(constraintString) {
	# a constraintString with the quantitative constraint to identify
	# "=0USD" or "> 1.77 EUR" or < 5% 
	
	string <- removeStartEndSpaces(unclass(constraintString))
	
	# check for operator
	start = 1
	if (grepl("^>=|<=|!=", string)) {
		stop=2
	} else {
		if (grepl("^>|<|=", string)) {
			stop=1
		} else {
			stop(paste("Error: missing operator:",string))
		}
	}
	operator <- substr(string,start,stop)
	
	string <- substr(string,stop+1,nchar(string))
	string <- removeStartEndSpaces(string)
	
	# check for currency
	if (grepl("[A-Z]{3}$", string)) {
		stop  = nchar(string)
		start = stop - 2
		
		kind = "absolute"
		currency <- substr(string,start,stop)
		string <- substr(string,1,start-1)
		amount <- as.numeric(string)
		money <- toMoney(amount,currency)
		
		constraint <- new("Constraint",
				operator=operator,value=money,kind=kind)
		
		return(constraint)
	} else {
		# check for %
		if (grepl("%$", string)) {
			kind = "relative"
			string <- substr(string,1,nchar(string)-1)
			amount <- as.numeric(string)
			
			constraint <- new("Constraint",
					operator=operator,value=amount,kind=kind)
			
			return(constraint)
		} else {
			stop("Error: missing currency or % identifier")
		}
	}
}
