# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldAddSpaces <- function() {
	
	# should return x without changes
	x <- "abcde"
	checkEquals(str_pad(x,width=2,side="right"),x)
	
	x <- "pippo"
	checkEquals(str_pad(x,width=6,side="right"),"pippo ")
	
	x <- "pippo"
	checkEquals(str_pad(x,width=10,side="right"),"pippo     ")
		
	x <- "pippo"
	checkEquals(str_pad(x,width=6,side="left")," pippo")
	
	x <- "pippo"
	checkEquals(str_pad(x,width=10,side="left"),"     pippo")
	
}
