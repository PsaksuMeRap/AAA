# TODO: Add comment
# 
# Author: Claudio
###############################################################################

tradeToSecurityFactory <- function(securityTrade) {
	# determine the security type
	securityType <- securityTrade$Security_type
	
	if (securityType=="Equity") {
		currency <- new("Currency",securityTrade$Currency)
		name <- securityTrade$Security_name
		id=new("IdBloomberg",securityTrade$Id_Bloomberg)
		
		newSecurity <- new("Equity",currency=currency,name=name,id=id) 
		return(newSecurity)		
	}

	if (securityType=="Future index") {
		currency <- new("Currency",securityTrade$Currency)
		name <- securityTrade$Security_name
		id=new("IdBloomberg",securityTrade$Id_Bloomberg)
		
		newSecurity <- new("Equity",currency=currency,name=name,id=id) 
		return(newSecurity)		
	}
	
}