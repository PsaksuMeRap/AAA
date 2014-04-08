# TODO: Add comment
# 
# Author: claudio
###############################################################################

explodeAndReplaceFundDynamicMultistrategyAllPortfolios <- function(portfolios,percentages) {

	return(new("Portfolios",lapply(portfolios,explodeAndReplaceFundDynamicMultistrategy,percentages)))
}


explodeAndReplaceFundDynamicMultistrategy <- function(portfolio,percentages) {

	# This procedure explodes and replace the security "Fondi_azionari" OnCapital Dynamic Multistrategy
	if (length(portfolio)==0) return(portfolio)
	
	isDynamicMultistrategyFund <- function(position) {return(position@security@id@idAAA=="LU0810450972")}
	isDynamicMultistrategyPosition <- sapply(portfolio,isDynamicMultistrategyFund)

	if (any(isDynamicMultistrategyPosition)) {
		owner <- portfolio@owner
		referenceCurrency <- portfolio@referenceCurrency
		replacements <- lapply(portfolio[isDynamicMultistrategyPosition],explodeFundDynamicMultistrategy,percentages)
		newPositions <- replacements[[1]]
		if (length(replacements)>1) {
			for (pos in replacements[-1]) newPositions <- join(newPositions,pos)
		}
		if (any(!isDynamicMultistrategyPosition)) newPositions <- join(newPositions,portfolio[!isDynamicMultistrategyPosition])
		
		return(new("Portfolio",owner=owner,referenceCurrency=referenceCurrency,newPositions))
	}
	return(portfolio)
}
		
