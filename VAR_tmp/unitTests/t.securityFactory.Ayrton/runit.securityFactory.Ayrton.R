# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailSecurityFactoryWithNonAyrton <- function() {
	
	# create the origin
	origin <- list()
	class(origin) <- "pippo"
	
	checkException(securityFactory(origin))
	
}

test.securityFactory.Ayrton.equity <- function() {
	
	source("./unitTests/utilities/allocateTestRepositories.R")	
	
	# create the equity repository and instrument repository
	allocateTestRepositories("equities")	
	allocateTestRepositories("instruments")	
	allocateTestRepositories("exchangeRates")
	
	securityFactory <- function(origin) UseMethod("securityFactory")
	
	securityFactory.default <- function(origin) {
		stop(paste("No suitable securityFactory method for origin of class",class(origin)))
	}
	
	securityFactory.ayrton <- function(origin) {
		
		ID_Ayrton <- new("ID_Ayrton",ID_AAA=origin[["ID_AAA"]],
				ID_strumento=origin[["ID_strumento"]])
		
		identifyInstrument <- function(record) {
			# record: a list
			
			# create the repository of the instruments if not available
			if (!exists("instruments",envir=repositories,inherits=FALSE)) {
				eval(expression(instruments <- create_repositoryInstruments())
						,env=repositories)
			}
			
			instrumentName <- repositories$instruments$getInstrumentName(record["ID_strumento"])
			if (is.na(instrumentName)) {
				msg <- paste("Attenzione: lo strumento di ID",
						record[["ID_strumento"]],"non esite!")
				stop(msg)
			}
			return(instrumentName)
		}
		
		instrumentName <- identifyInstrument(origin)
		if (identical(instrumentName,"equity")) {
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

	equity <- securityFactory(origin)
	
	checkEquals(equity@name,"Roche Holding Gs")
	checkEquals(equity@ID,new("ID_Ayrton",ID_AAA=824,ID_strumento=1))
	
	# restore initial conditions
	deallocateTestRepositories("exchangeRates")		
	deallocateTestRepositories("equities")	
	deallocateTestRepositories("instruments")	
}
