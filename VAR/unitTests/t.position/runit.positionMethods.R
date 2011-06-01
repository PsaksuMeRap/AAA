# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.positionCopy <- function() {
	source("./lib/position/position.R")
	source("./lib/repository.R")
	
	allocateTestRepositories("equities")
	
	# crea la posizione
	position <- create_position()
	position$create(name="Deutsche Telekom Common Stock",
			currency="EUR",
			amount=0.0,
			origin=list(ID_AAA=418,esempio="Claudio")
	)
	class(position) <- c("test",class(position))
	
	
	# crea la nuova posizione e copia la vecchia
	newPosition <- copyPosition(position)
	checkEquals(class(position),class(newPosition))	
	checkEquals(position$name,newPosition$name)
	checkEquals(position$money,newPosition$money)
	checkEquals(position$origin,newPosition$origin)
	
	deallocateTestRepositories("equities")
}
