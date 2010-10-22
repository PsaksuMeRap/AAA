# TODO: Add comment
# 
# Author: claudio
###############################################################################



positionsSelector <- function(criterium,positions) {
	# positions: the positions on which to apply the check	
	UseMethod("positionsSelector",criterium)
}

positionsSelector.currency <- function(currency,positions){
	
	trueFalse <- lapply(positions,
			function(position,currency) return(position$isCurrency(currency)),
			currency)
	trueFalse <- unlist(trueFalse)

	return(positions[trueFalse])
}

positionsSelector.instrument <- function(instrument,positions) {
	
	trueFalse <- lapply(positions,
			function(position,instrument) return(position$isInstrument(instrument)),
			instrument)
	trueFalse <- unlist(trueFalse)
	
	return(positions[trueFalse])	
}


create_criterium <- function(value,criteriumClass) {
	
	class(value) <- criteriumClass
	return(value)
}
# analizza i nomi e guarda se funzionano correttamente. crea un repository per le
# criteriumClass con i rispettivi valori? Esempio
# instruments: equity, bond, ...
# currency: usd, chf, eur
# geograficArea: EU, USA, Canada, Asia, Japan



