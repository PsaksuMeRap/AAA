# TODO: Add comment
# 
# Author: claudio
###############################################################################



relationalOperatorFactory <- function(string) {
	# string: a string with possibly but not necessarily, at the beginning, a relational operator
	# "=xxx" or "> Baa+" or "<= 5%" or "AAA"

	string <- removeStartEndSpaces(string)
	nbChar <- nchar(string)
	
	# check for operator
	start = 1
	if (grepl("^>=|<=|!=", string)) {
		stop=2
	} else {
		if (grepl("^>|<|=", string)) {
			stop=1
		} else {
			stop=0
		}
	}
	
	if (stop) operator <- substr(string,start,stop) else operator <- ""
	
	if (nbChar==stop) return(c(operator=operator,values=""))

	values <- substr(string,stop+1,nbChar)

	return(c(operator=operator,values=removeStartEndSpaces(values)))
}
