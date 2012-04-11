# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailWithNA <- function() {
	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	equity1 <- repository$equity1
	equity1@ID_AAA <-NA_real_
	
	
}
