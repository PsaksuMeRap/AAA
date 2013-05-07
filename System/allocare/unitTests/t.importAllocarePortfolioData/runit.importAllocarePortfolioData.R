# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldImportFile <- function() {
	
	# test empty file
	filename = "./allocare/unitTests/data/portafogli_allocare_empty.txt"
	result <- importAllocarePortfolioData(filename)
	checkEquals(length(result),0)
	
	# test non empty file
	filename = "./allocare/unitTests/data/portafogli_allocare.txt"
	result <- importAllocarePortfolioData(filename)
	checkEquals(length(result)>0,TRUE)
}
