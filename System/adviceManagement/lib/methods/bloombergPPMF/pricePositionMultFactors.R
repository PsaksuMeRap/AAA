# TODO: Add comment
# 
# Author: Claudio
###############################################################################


create_chfMultFactors <- function() {
	## remove the CHF from the list of allowedCurrencies
	isCHF <- sys[["allowedCurrencies"]] == "CHF"
	allowedCurrencies <- sys[["allowedCurrencies"]][!isCHF]

	securities <- paste(allowedCurrencies,"CHF Curncy",sep="")
	conn <- blpConnect()
	result <- bdp(conn, securities=securities, fields="PX_POS_MULT_FACTOR")
	blpDisconnect(conn)
	
	currCodes <- substr(rownames(result),1,6)
	multFactors <- result[["PX_POS_MULT_FACTOR"]]
	names(multFactors) <- currCodes
	return(multFactors)
}

