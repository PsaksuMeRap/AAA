# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldRemoveSpaces <- function() {
	
	x <- ""
	checkEquals(remSpaces(x),x)
	
	x <- "   "
	checkEquals(remSpaces(x),"")
	
	x <- "x"
	checkEquals(remSpaces(x),x)
	
	x <- "    x    "
	checkEquals(remSpaces(x),"x")
	
	x <- " a b c"
	checkEquals(remSpaces(x),"a b c")
	
}
