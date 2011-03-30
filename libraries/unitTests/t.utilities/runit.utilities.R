# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.trimString <- function() {
	## test 1
	string <- "  Claudio Ortelli\n pippo a"
	should <- "Claudio Ortelli\n pippo a"
	
	result <- trim(string)
	checkEquals(result,should)
	
	## test 2	
	string <- "Ortelli\n pippo a    "
	should <- "Ortelli\n pippo a"
	
	result <- trim(string)
	checkEquals(result,should)
	
	## test 3
	string <- "  \n   Ortelli\n pippo a    \n"
	should <- "\n   Ortelli\n pippo a    \n"
	
	result <- trim(string)
	checkEquals(result,should)
	
	## test 4
	string <- "  \n   Ortelli\n pippo a    \n     "
	should <- "\n   Ortelli\n pippo a    \n"
	
	result <- trim(string)
	checkEquals(result,should)
}