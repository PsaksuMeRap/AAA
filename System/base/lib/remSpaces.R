# TODO: Add comment
# 
# Author: claudio
###############################################################################


remSpaces <- function(x) {
	return(sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)) 
}
