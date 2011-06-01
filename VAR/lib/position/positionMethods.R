# TODO: Add comment
# 
# Author: claudio
###############################################################################


copyPosition <- function(position) {
	newPosition <- create_position()
	# crea la nuova posizione e copia la vecchia
	NewPosition <- create_position()
	newPosition$create(name=position$name,
			currency=position$money$currency,
			amount=position$money$amount,
			origin=position$origin
	)
	class(newPosition) <- class(position)
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