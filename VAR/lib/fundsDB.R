# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_fundsDB <- function() {
	# i fondi e le relative informazioni sono
	# inseriti manualmente nel seguente data.frame
	
	nomeFondo <- c("GLOBAL EQUITY","GLOBAL ECONOMY","FIXED INCOME")
	numeroValore <- c("2742261CH","11995588CH","2490099")
	owner <- c("pippo53","pippo210","pippo76")
	
	fundsDB <- data.frame(
			nomeFondo = nomeFondo,
			numeroValore = numeroValore,
			owner = owner,
			stringsAsFactors=FALSE
	)
	return(fundsDB)
}

