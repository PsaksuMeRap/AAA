# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldCreateSelectionCriteriaFromFactorStrings <- function() {
	
	factorStrings <- new("FactorStrings","security:Equity,Bond & currency:CHF & amount:>50CHF")
	factorString1 <- new("FactorString","security:Equity,Bond")
	factorString2 <- new("FactorString","currency:CHF")
	factorString3 <- new("FactorString","amount:>50CHF")
	
	selCrit1 <- selectionCriteriumFactory(split(factorString1))
	selCrit2 <- selectionCriteriumFactory(split(factorString2))
	selCrit3 <- selectionCriteriumFactory(split(factorString3))

	should <- new("SelectionCriteria",list(selCrit1,selCrit2,selCrit3))
	result <- toSelectionCriteria(factorStrings)
	checkEquals(result,should)
}
