# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_fundsDB <- function() {
	# i fondi e le relative informazioni sono
	# inseriti manualmente nel seguente data.frame
	
	nomeFondo <- c("GLOBAL EQUITY","GLOBAL ECONOMY","FIXED INCOME")
	instrumentClass <- c("Fondi_azionari","Fondi_azionari","Fondi_obbligazionari")
	id <- c("1701","2256","825")
	owner <- c("pippo53","pippo210","pippo76")


	fundsDB <- data.frame(
			nomeFondo = nomeFondo,
			instrumentClass,
			id = id,
			owner = owner,
			stringsAsFactors=FALSE
	)
	return(fundsDB)
}

