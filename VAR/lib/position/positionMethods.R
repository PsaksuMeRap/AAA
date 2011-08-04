# TODO: Add comment
# 
# Author: claudio
###############################################################################


copyPosition <- function(position) {
	parser <- create_parserPosition()
	newPosition <- parser$parse(position$origin)
	return(newPosition)
}

weightPosition <- function(position,weight) {
	position$money$amount <- weight * position$money$amount
	position$origin$Saldo <- weight * position$origin$Saldo
	position$origin$ValorePosizione <- weight * position$origin$ValorePosizione			
	position$origin$ValoreMonetaRiferimento <- weight * position$origin$ValoreMonetaRiferimento	
	position$origin$ValoreMercatoMonetaCHF <- weight * position$origin$ValoreMercatoMonetaCHF						
	position$origin$ValoreMercatoMonetaEUR <- weight * position$origin$ValoreMercatoMonetaEUR					
	position$origin$ValoreMercatoMonetaUSD <- weight * position$origin$ValoreMercatoMonetaUSD
}