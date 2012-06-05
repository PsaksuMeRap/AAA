# TODO: Add comment
# 
# Author: Claudio
###############################################################################

tradeToSecurityFactory <- function(trade) {
	# determine the security type
	securityType <- trade$Security_type
	
	if (securityType=="Equity") {
		currency <- new("Currency",trade$Currency)
		name <- trade$Security_name
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		newSecurity <- new("Equity",currency=currency,name=name,id=id) 
		return(newSecurity)		
	}

	if (securityType=="Future index") {
		currency <- new("Currency",trade$Currency)
		name <- trade$Security_name
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		
		
		newSecurity <- new("Future_EQ",currency=currency,name=name,id=id,underlying=underlying) 
		return(newSecurity)		
	}
	
}