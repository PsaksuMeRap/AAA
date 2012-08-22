# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldImportAndCreatePPMFvsCHF <- function() {
	## PPMF = Price Position Multiplicator Factors
	currencies <- c("AUD","DKK","IDR","JPYCHF")
	
	multFactors <- create_chfMultFactors(currencies)
	shouldMultFactors <- c("AUDCHF"=1,"DKKCHF"=0.01,"IDRCHF"=0.0001,"JPYCHF"=0.01)
	
	checkEquals(multFactors,shouldMultFactors)

}
