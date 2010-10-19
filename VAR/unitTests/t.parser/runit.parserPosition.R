# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.parseEquityPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	# create the equity repository
	source("./lib/repository.R")
	equityRepository <- create_repositoryEquities(equities.df)
    
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
    assign("equities",equityRepository,envir=repositories)
    assign("instruments",instrumentsRepository,
			envir=repositories)

	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")
	dati.df <- createPositionsData()
	
	equity <- parser$parse(dati.df[2,])
	
	checkEquals(is.element("equity",class(equity)),TRUE)
	checkEquals(equity$ticker,"PHIA.AS")
	
}


test.parseBondPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	# create the equity repository
	source("./lib/repository.R")
	equityRepository <- create_repositoryEquities(equities.df)
	
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
	assign("equities",equityRepository,envir=repositories)
	assign("instruments",instrumentsRepository,
			envir=repositories)
	
	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")	
	dati.df <- createPositionsData()
	
	bond <- parser$parse(dati.df[700,])
	
	checkEquals(is.element("bond",class(bond)),TRUE)
}



