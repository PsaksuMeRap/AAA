# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_equityTestPositions <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	
	allocateTestRepositories("equities")
	exists("equities",envir=repositories,inherits=FALSE)
	
	# crea positions
	positions.l <- list()
	
	# crea la posizione 1
	position <- create_position()
	position$create(name="Credit Suisse Group Na",
			currency="CHF",
			amount=100.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=368))
	positions.l[["equityCHF1"]] <- position
	
	# crea la posizione 2 
	position <- create_position()
	position$create(name="SUNCOR ENERGY  INC. CO",
			currency="USD",
			amount=124.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=1984))
	positions.l[["equityUSD1"]] <- position
	
	# crea la posizione 3
	position <- create_position()
	position$create(name="DT Telekom N",
			currency="EUR",
			amount=98.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=418))
	positions.l[["equityEUR1"]] <- position
	
	# crea la posizione 4
	position <- create_position()
	position$create(name="Nestle Na",
			currency="CHF",
			amount=13.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=697))
	positions.l[["equityCHF2"]] <- position
	
	
	deallocateTestRepositories("equities")
	return(positions.l)
}


create_ETF_equityTestPositions <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# crea positions
	positions.l <- list()
	
	# crea la posizione
	position <- create_position()
	position$create(name="iShares DAX (DE)",
			currency="EUR",
			amount=100.0
	)
	class(position) <- c("ETF_equity","position")
	extendPosition(position,origin=list(ID_AAA=1482))
	positions.l[["ETF_equityEUR1"]] <- position
	
	# crea la posizione
	position <- create_position()
	position$create(name="ISHARES INC MSCI South Korea",
			currency="USD",
			amount=174.0
	)
	class(position) <- c("ETF_equity","position")
	extendPosition(position,origin=list(ID_AAA=1706))
	positions.l[["ETF_equityUSD1"]] <- position
	
	# crea la posizione
	position <- create_position()
	position$create(name="CQ XMTCH ON SMI",
			currency="CHF",
			amount=98.0
	)
	class(position) <- c("ETF_equity","position")
	extendPosition(position,origin=list(ID_AAA=1264))
	positions.l[["ETF_equityCHF1"]] <- position
	
	return(positions.l)
}


create_Conto_correnteTestPositions <- function() {
	positions.l <- list()
	
	# create position CHF
	position <- create_position()
	position$create(name="Conto corrente CHF",currency="CHF",
			amount=120.0)
	class(position) <- c("Conto_corrente",class(position))
	positions.l[["Conto_correnteCHF1"]] <- position
	
	# create position USD
	position <- create_position()
	position$create(name="Conto corrente USD",currency="USD",
			amount=12.0) 
	class(position) <- c("Conto_corrente",class(position))
	positions.l[["Conto_correnteUSD1"]] <- position
	
	# create position
	position <- create_position()
	position$create(name="Conto corrente EUR",currency="EUR",
			amount=10.0)
	class(position) <- c("Conto_corrente",class(position))
	positions.l[["Conto_correnteEUR1"]] <- position
	
	return(positions.l)
}


create_FX_ForwardTestPositions <- function() {
	positions.l <- list()
	
	# create position CHF
	position <- create_position()
	position$create(name="CHF 6,590,000.00 Valuta 01-02-2011",currency="CHF",
			amount=120.0)
	class(position) <- c("FX_Forward",class(position))
	extendPosition(position,origin=NA)
	positions.l[["FX_ForwardCHF1"]] <- position
	
	# create position USD
	position <- create_position()
	position$create(name="EUR -5,000,000.00 Valuta 01-02-2011",currency="EUR",
			amount=12.0) 
	class(position) <- c("FX_Forward",class(position))
	extendPosition(position,origin=NA)
	positions.l[["FX_ForwardEUR1"]] <- position
	
	return(positions.l)
}


