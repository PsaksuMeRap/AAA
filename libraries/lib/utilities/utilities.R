# TODO: Add comment
# 
# Author: claudio
###############################################################################

trim <- function(string)
{
	## remove leading and ending white spaces
	string <- sub(pattern="^ +", replacement="", x=string)
	string <- sub(pattern=" +$", replacement="", x=string)
	return(string)
}

