# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.positionCopy <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./lib/position/position.R")
	source("./lib/repository.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	
	# crea la posizione
	position <- create_position()
	position$create(name="Deutsche Telekom Common Stock",
			currency="EUR",
			amount=0.0,
			origin=list(
					ID_AAA=418,
					esempio="Claudio",
					ValoreMercatoMonetaCHF=0.0,
					Moneta="EUR",
					Cliente="pippo160",
					Strumento= "A",
					Nome="Deutsche Telekom Common Stock",
					ID_strumento=1
				)
	)
	class(position) <- c("equity",class(position))
	extendPosition.equity(position)
	
	# crea la nuova posizione e copia la vecchia
	newPosition <- copyPosition(position)
	checkEquals(class(position),class(newPosition))	
	checkEquals(position$name,newPosition$name)
	checkEquals(position$money,newPosition$money)
	checkEquals(position$origin,newPosition$origin)
	
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")
}
