# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_fundsDB <- function() {
	# i fondi e le relative informazioni sono
	# inseriti manualmente nel seguente data.frame
	
	nomeFondo <- c("GLOBAL EQUITY","GLOBAL ECONOMY","FIXED INCOME")
	id <- c("1701","123456","825")
	owner <- c("pippo53","pippo210","pippo76")
print(rep("correggere id per Global Economy: da fundsDB.R!!",10))


	fundsDB <- data.frame(
			nomeFondo = nomeFondo,
			id = id,
			owner = owner,
			stringsAsFactors=FALSE
	)
	return(fundsDB)
}

