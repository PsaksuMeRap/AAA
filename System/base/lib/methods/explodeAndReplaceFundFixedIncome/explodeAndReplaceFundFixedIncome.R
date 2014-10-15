# TODO: Add comment
# 
# Author: claudio
###############################################################################

explodeAndReplaceFundFixedIncomeAllPortfolios <- function(portfolios,percentages) {

	return(new("Portfolios",lapply(portfolios,explodeAndReplaceFundFixedIncome,percentages)))
}


explodeAndReplaceFundFixedIncome <- function(portfolio,percentages) {

	# This procedure explodes and replace the security "Fondi_obbligazioniari" OnCapital Fixed Income
	if (length(portfolio)==0) return(portfolio)
	
	isFixedIncomeFund <- function(position) {return(position@security@id@idAAA=="LU0810451608")}
	isFixedIncomePosition <- sapply(portfolio,isFixedIncomeFund)

	if (any(isFixedIncomePosition)) {
		owner <- portfolio@owner
		referenceCurrency <- portfolio@referenceCurrency
		replacements <- lapply(portfolio[isFixedIncomePosition],explodeFundFixedIncome,percentages)
		newPositions <- replacements[[1]]
		if (length(replacements)>1) {
			for (pos in replacements[-1]) newPositions <- join(newPositions,pos)
		}
		if (any(!isFixedIncomePosition)) newPositions <- join(newPositions,portfolio[!isFixedIncomePosition])
		
		return(new("Portfolio",owner=owner,referenceCurrency=referenceCurrency,newPositions))
	}
	return(portfolio)
}
		
