# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.securityFactory <- function() {
	
	securityFactory <- function(origin,...) UseMethod("securityFactory")
	
	securityFactory.default <- function(origin,...) {
		stop(paste("No suitable securityFactory method for origin of class",class(origin)))
	}
	
	securityFactory.ayrton <- function(origin,securityName) {
		
		if (identical(securityName,"equity")) {
			ID_Ayrton <- new("ID_Ayrton",ID_AAA=origin[["ID_AAA"]],
					ID_strumento=origin[["ID_strumento"]])
			equity <- new("Equity",name=origin[["Nome"]],ID=ID_Ayrton)
			
			return(equity)
		}
		
		stop(paste("Error: unknown securityName",securityName))
	}
	

	# create the origin
	origin <- list()
	origin$Nome <- "Roche Holding Gs"
	origin$ID_AAA <- 824
	origin$ID_strumento <- 1
	class(origin) <- "ayrton"

	equity <- securityFactory(origin,"equity")
	
	
}
