# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEquitySecurityFromIsin <- function(isin,currencyCode="")  {

	isOk <- repositories$DBEquities$DBEquities.df[,"ISIN"] == isin
	
	matches <- sum(isOk)
	if (matches==0) return(new("Equity",name="Equity not in DBEquity",id=new("IdCharacter",isin)))
	
	if (matches==1) {
		result <- repositories$DBEquities$DBEquities.df[isOk,]
	} else {
		result <- repositories$DBEquities$DBEquities.df[isOk,]
		is_desiredCurrency <- result[,"Moneta"]==currencyCode
		if (!any(is_desiredCurrency)) {
			stop(paste("Error from createEquitySecurityFromIsin.",
							"Two equities with same ISIN code and no currency matching. ISIN equal:",
							isin))
		} else {
			result <- result[is_desiredCurrency,]
		}
	}
	
	currency <- new("Currency",result[["Moneta"]])
	
	# create the id field as in idFactory
	id <- new("IdAyrton",
			idAAA=new("IdAAA_character",isin),
			idStrumento=result[["ID_strumento"]])
	
	name <- result[["Azione"]]
	
	return(new("Equity",currency=currency,name=name,id=id))
	
}
