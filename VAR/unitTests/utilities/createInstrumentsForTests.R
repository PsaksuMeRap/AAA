# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_equityTestPositions <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	
	allocateTestRepositories("equities")
	exists("equities",envir=repositories,inherits=FALSE)
	
	# crea positions
	positions <- create_positions()
	
	# crea la posizione 1
	position <- create_position()
	position$create(name="Credit Suisse Group Na",
			currency="CHF",
			amount=100.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=368,NumeroValore="1213853CH"))
	positions$add(position)
	
	# crea la posizione 2 
	position <- create_position()
	position$create(name="SUNCOR ENERGY  INC. CO",
			currency="USD",
			amount=124.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=1984,NumeroValore="CA8672241079"))
	positions$add(position)
	
	# crea la posizione 3
	position <- create_position()
	position$create(name="DT Telekom N",
			currency="EUR",
			amount=98.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=418,NumeroValore="1026592EU"))
	positions$add(position)
	
	# crea la posizione 4
	position <- create_position()
	position$create(name="Nestle Na",
			currency="CHF",
			amount=13.0
	)
	class(position) <- c("equity","position")
	extendPosition(position,origin=list(ID_AAA=697,NumeroValore="3886335CH"))
	positions$add(position)
	
	
	deallocateTestRepositories("equities")
	return(positions)
}


create_ETF_equityTestPositions <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# crea positions
	
	positions <- create_positions()
	
	# crea la posizione
	position <- create_position()

	position$create(name="iShares DAX (DE)",
			currency="EUR",
			amount=100.0#,
			#origin=list(ID_AAA=1482)
	)
	
	class(position) <- c("ETF_equity","position")
	extendPosition(position,origin=list(ID_AAA=1482))
	positions$add(position)
	
	# crea la posizione
	position <- create_position()

	position$create(name="ISHARES INC MSCI South Korea",
			currency="USD",
			amount=174.0#,
			#origin=list(ID_AAA=1706)
	)
	
	class(position) <- c("ETF_equity","position")
	extendPosition(position,origin=list(ID_AAA=1706))
	positions$add(position)
	
	# crea la posizione
	position <- create_position()
	
	position$create(name="CQ XMTCH ON SMI",
			currency="CHF",
			amount=98.0#,
			#origin=list(ID_AAA=1264)
	)
	
	class(position) <- c("ETF_equity","position")
	extendPosition(position,origin=list(ID_AAA=1264))
	positions$add(position)
	
	return(positions)
}


create_Conto_correnteTestPositions <- function() {
	positions <- create_positions()
	
	# create position CHF
	position <- create_position()
	position$create(name="Conto corrente CHF",currency="CHF",
			amount=120.0) #,origin=NA)
	class(position) <- c("Conto_corrente",class(position))
	positions$add(position)
	
	# create position USD
	position <- create_position()
	position$create(name="Conto corrente USD",currency="USD",
			amount=12.0) #,origin=NA)
	class(position) <- c("Conto_corrente",class(position))
	positions$add(position)
	
	# create position
	position <- create_position()
	position$create(name="Conto corrente EUR",currency="EUR",
			amount=10.0) #,origin=NA)
	class(position) <- c("Conto_corrente",class(position))
	positions$add(position)
	
	return(positions)
}


create_FX_ForwardTestPositions <- function() {
	positions <- create_positions()
	
	# create position CHF
	position <- create_position()
	position$create(name="CHF 6,590,000.00 Valuta 01-02-2011",currency="CHF",
			amount=120.0) #,origin=NA)
	class(position) <- c("FX_Forward",class(position))
	positions$add(position)
	extendPosition(position,origin=NA)
	
	# create position USD
	position <- create_position()
	position$create(name="EUR -5,000,000.00 Valuta 01-02-2011",currency="EUR",
			amount=12.0) #,origin=NA)
	class(position) <- c("FX_Forward",class(position))
	positions$add(position)
	extendPosition(position,origin=NA)
	
	return(positions)
}


