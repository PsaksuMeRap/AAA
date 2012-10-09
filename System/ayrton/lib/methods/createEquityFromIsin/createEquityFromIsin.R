# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEquitySecurityFromIsin <- function(isin)  {
	
	isOk <- repositories$DBEquities$DBEquities.df[,"ISIN"] == isin
	
	matches <- sum(isOk)
	if (matches==0) return(new("Equity",name="Equity not in DBEquity",id=new("IdCharacter",isin)))
	
	if (matches==1) {
		result <- repositories$DBEquities$DBEquities.df[isOk,]
	} else {
		stop(paste("Error from createEquitySecurityFromIsin.",
						"Two equities with same ISIN code",
						isin))
	}
	
	currency <- new("Currency",result[["Moneta"]])
	
	# create the id field as in idFactory
	id <- new("IdAyrton",
			idAAA=new("IdAAA_character",isin),
			idStrumento=result[["ID_strumento"]])
	
	name <- result[["Azione"]]
	
	return(new("Equity",currency=currency,name=name,id=id))
	
}
