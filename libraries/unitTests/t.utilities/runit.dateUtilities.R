# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.completeDatePart <- function() {
	## test 1
	string <- c("1","03","12","",NA)
	should <- c("01","03","12","",NA)
	
	result <- completeDatePart(string)
	checkEquals(result,should)
}
