# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldImportDBEquities <- function() {
	## saveLastObject(repository$DBEquities,fileName="DBEquities.RData",directory="./adviceManagement/unitTests/files/riskman/data/DBEquities")
	
	# remove the DBEquities if exists
	
	if (exists("DBEquities",repositories)) rm(DBEquities,envir=repositories) 
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data","DBEquities")
	fileName <- "DBEquities.RData"

	tmpEnvir <- new.env()
	load(file.path(directory,fileName),envir=tmpEnvir)
	repositories$DBEquities <- list()
	class(repositories$DBEquities) <- "repositoryDBEquities"
	repositories$DBEquities[["DBEquities.df"]] <- tmpEnvir$object
	
	
}
