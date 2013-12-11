# TODO: Add comment
# 
# Author: claudio
###############################################################################

explodeAndReplaceFundGlobalEquityAllPortfolios <- function(portfolios,percentages) {

	return(new("Portfolios",lapply(portfolios,explodeAndReplaceFundGlobalEquity,percentages)))
}


explodeAndReplaceFundGlobalEquity <- function(portfolio,percentages) {

	# This procedure explodes and replace the security "Fondi_azionari" OnCapital Global Equity
	if (length(portfolio)==0) return(portfolio)
	
	isGlobalEquityFund <- function(position) {return(position@security@id@idAAA=="LU0810451434")}
	isGlobalEquityPosition <- sapply(portfolio,isGlobalEquityFund)

	if (any(isGlobalEquityPosition)) {
		owner <- portfolio@owner
		referenceCurrency <- portfolio@referenceCurrency
		replacements <- lapply(portfolio[isGlobalEquityPosition],explodeFundGlobalEquity,percentages)
		newPositions <- replacements[[1]]
		if (length(replacements)>1) {
			for (pos in replacements[-1]) newPositions <- join(newPositions,pos)
		}
		if (any(!isGlobalEquityPosition)) newPositions <- join(newPositions,portfolio[!isGlobalEquityPosition])
		
		return(new("Portfolio",owner=owner,referenceCurrency=referenceCurrency,newPositions))
	}
	return(portfolio)
}
		
