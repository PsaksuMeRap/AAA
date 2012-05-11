# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldProcessNewAdviceMessage <- function() {

}


test.shouldProcessAdviceConfirmationMessage <- function() {
	
}


test.shouldProcessPreComplianceResultMessage <- function() {
	
	setMethod("messageProcessing",signature(message="PreComplianceResult"),
		function(message) {

		# identify the success or failure ot the test
		if (message[["testResult"]]=="no") {
			#a) sendEmail, 
			#b) move newAdvice and preComplianceResult da pending a archive/processed/rejected, 
			#c) rimuovi il lock
			return()
		}

		#+ risultato positivo:
		#a) Mantieni il lock
		#b) invia e-mail
		#c) lascia il newAdvice nel pending
		#d) sposta il preCompliance nel pending
		#e) registra nella coda del processo main che stai attendendo il file di conferma eseguito

}
}

test.shouldProcessPostComplianceResultMessage <- function() {
	
}