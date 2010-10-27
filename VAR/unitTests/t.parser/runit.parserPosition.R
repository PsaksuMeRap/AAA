# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldParseEquityPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	# create the equity repository
	source("./lib/repository.R")
	equityRepository_tmp <- create_repositoryEquities(equities.df)
    
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository_tmp <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
	equityRepository <- repositories$equities
    assign("equities",equityRepository_tmp,envir=repositories)
	instrumentsRepository <- repositories$instruments
    assign("instruments",instrumentsRepository_tmp,
			envir=repositories)

	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")
	dati.df <- createPositionsData()
	
	equity <- parser$parse(dati.df[2,])
	
	checkEquals(is.element("equity",class(equity)),TRUE)
	checkEquals(equity$ticker,"PHIA.AS")
	
	# restore initial conditions
	assign("equities",equityRepository,envir=repositories)
	assign("instruments",instrumentsRepository,
			envir=repositories)
}


test.shouldParseBondPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository_tmp <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
	instrumentsRepository <- repositories$instruments
	assign("instruments",instrumentsRepository_tmp,
			envir=repositories)
	
	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")	
	dati.df <- createPositionsData()
	
	bond <- parser$parse(dati.df[700,])
	
	checkEquals(is.element("bond",class(bond)),TRUE)
	
    # restore initial conditions
	assign("instruments",instrumentsRepository,
			envir=repositories)
}



test.shouldIdenfyAccruedInterest <- function() {
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository_tmp <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
	instrumentsRepository <- repositories$instruments
	assign("instruments",instrumentsRepository_tmp,
			envir=repositories)
	
	
	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")	
	dati.df <- createPositionsData()
	
	bond <- parser$parse(dati.df[493,])
	
	checkEquals(is.element("bond",class(bond)),TRUE)
	
	# restore initial conditions
	assign("instruments",instrumentsRepository,
			envir=repositories)
	
}

