# TODO: Add comment
# 
# Author: claudio
###############################################################################



constraintFactory <- function(constraintString) {
	# a constraintString with the quantitative constraint to identify
	# "=0USD" or "> 1.77 EUR" or < 5% 
	if (class(constraintString)!="ConstraintString") {
		message <- "constraintFactory requires a ConstraintString!"
		paste(message,"\nGot",class(constraintString),"instead.")
		stop(message)
	}

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
		
		# it is a kind = "absolute" constraint 
		currency <- substr(string,start,stop)
		string <- substr(string,1,start-1)
		amount <- as.numeric(string)
		money <- toMoney(amount,currency)
		
		constraint <- new("AbsoluteConstraint",
				operator=operator,value=money)
		
		return(constraint)
	} else {
		# check for %
		if (grepl("%$", string)) {
			# it is a kind = "relative" constraint
			string <- substr(string,1,nchar(string)-1)
			amount <- as.numeric(string)
			
			constraint <- new("RelativeConstraint",
					operator=operator,value=amount)
			
			return(constraint)
		} else {
			stop("Error: missing currency or % identifier")
		}
	}
}
