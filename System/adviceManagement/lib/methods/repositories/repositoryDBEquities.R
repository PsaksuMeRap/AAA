# TODO: Add comment
# 
# Author: claudio
###############################################################################

import_repositoryDBEquitites <- function() {
	connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
	query = paste("SELECT A.ID, A.ID_strumento,A.Azione,A.NumeroValore,A.Moneta,A.ISIN FROM [Sistema (prova)].dbo.DBEquitites AS A",
			"INNER JOIN DBPortfolioGenerale AS B ON A.NumeroValore=B.NumeroValore")
	
	DBPortfolioGenerale.df <- sqlQuery(connection,query,as.is=TRUE)
	
}

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

