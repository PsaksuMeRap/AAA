# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldGroupById <- function() {
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$Futures_EQ1,repository$equity2,repository$bond1)
	
	
	groupBy <- function(positions,groupCriteria) {
		## positions: a variable of class positions
		## groupCriteria: a character vector of criteria to be used to aggregate
		## at the moment only "securityId" implemented
		if (length(positions)<=1) return(positions)
		
		if (groupCriteria=="securityId") {
			extractId <- function(position) {
				return(position@security@id)
			}
			ids <- lapply(positions,extractId)
			ids <- unlist(lapply(ids,idForGroupBy))
			
			result <- split(1:length(ids),as.factor(ids))

		}
	}
	
	# check with an empty list of Positions
	result <- groupBy(new("Positions"),"securityId")
	checkEquals(result,new("Positions"))
	
	# check with a Positions variable with 1 element
	positions <- new("Positions",list(repository$equity1))
	result <- groupBy(positions,"securityId")
	checkEquals(result,positions)
	
	# check with a Positions variable with 2 elements
#	positions <- new("Positions",list(repository$equity1,repository$equity1))
#	result <- groupBy(positions,"securityId")
#	checkEquals(result,positions)
}
