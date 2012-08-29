# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_repositoryDBEquities <- function() {
	
	if (exists("testFramework")) {
		directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data","DBEquities")
	} else {
		directory <- file.path(sys[["homeDir"]],"data","DBEquities")
	}
	
	fileName <- "DBEquities.RData"
	
	# create the list containing the data.frame DBEquities
	DBEquities <- list()
	
	# load the DBEquities.df
	tmpEnvir <- new.env()
	load(file.path(directory,fileName),envir=tmpEnvir)
	
	class(DBEquities) <- "repositoryDBEquities"
	DBEquities[["DBEquities.df"]] <- tmpEnvir$object
	
	# assign the repository	
	assign("DBEquities",DBEquities,envir=repositories)

}

create_repositoryDBEquities()

