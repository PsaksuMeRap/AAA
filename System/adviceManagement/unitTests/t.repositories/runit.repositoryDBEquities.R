# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldImportDBEquities <- function() {
	## saveLastObject(repository$DBEquities,fileName="DBEquities.RData",directory="./adviceManagement/unitTests/files/riskman/data/DBEquities")
	
	# remove the DBEquities if exists
	
	if (exists("DBEquities",repositories)) {
		backup <- repositories[["DBEquities"]]
		rm(DBEquities,envir=repositories) 
	}
	
	create_repositoryDBEquities()
	
	checkEquals(exists("DBEquities",env=repositories),TRUE)
	
	if (exists("backup",inherits=FALSE)) repositories[["DBEquities"]] <- backup
}
