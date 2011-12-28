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
			amount=0.0
	)
	
	origin=list()
	origin$ID_AAA <-418
	origin$esempio <-"Claudio"
	origin$ValoreMercatoMonetaCHF <- 0.0
	origin$Moneta <- "EUR"
	origin$Cliente <- "pippo160"
	origin$Strumento <- "A"
	origin$Nome <- "Deutsche Telekom Common Stock"
	origin$ID_strumento <- 1

#	class(position) <- c("equity",class(position))
#	extendPosition(position,origin)
	parser <- create_parserPosition()
	position <- parser$parse(origin)
	
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
