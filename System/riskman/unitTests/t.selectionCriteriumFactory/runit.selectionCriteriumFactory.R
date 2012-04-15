# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.selectionCriteriumFactory <- function() {
		
	# crea una checkString
	checkString = paste("amount:=10% + currency:JPY,EUR,USD +",
			"  amount:<=100.3CHF + instrument:xyz")
	checkString <- new("CheckString",checkString)
	result <- split(checkString)
	
	# test a security selection criterium	
	factorString <- new("FactorString","security:bond,equity")
	factorStringParsed <- split(factorString)
	
	SC1 <- selectionCriteriumFactory(factorStringParsed)
	
	checkEquals(SC1@values,c("bond","equity"))
	checkEquals(SC1@negation,FALSE)
	
	# test a currency selection criterium	
	factorString <- new("FactorString","currency!:JPY,EUR,USD")
	factorStringParsed <- split(factorString)
	
	SC1 <- selectionCriteriumFactory(factorStringParsed)
	
	checkEquals(SC1@values,c("JPY","EUR","USD"))
	checkEquals(SC1@negation,TRUE)
	
	# test an exception, i.e. non existing selection criterium	
	factorString <- new("FactorString","ideal:equity")
	factorStringParsed <- split(factorString)
	
	checkException(selectionCriteriumFactory(factorStringParsed))
	
	# test an absolute amount selection criterium	
	factorString <- new("FactorString","amount:<=100.3CHF")
	factorStringParsed <- split(factorString)
	
	SC1 <- selectionCriteriumFactory(factorStringParsed)
	
	checkEquals(SC1@constraint,
			new("AbsoluteConstraint",operator="<=",value=toMoney(100.3,"CHF"))
	)
	
	checkEquals(SC1@negation,FALSE)

	# test a relative amount selection criterium	
	factorString <- new("FactorString","amount!:=10%")
	factorStringParsed <- split(factorString)
	
	SC1 <- selectionCriteriumFactory(factorStringParsed)
	
	checkEquals(SC1@constraint,
			new("RelativeConstraint",operator="=",value=10)
	)
	
	checkEquals(SC1@negation,TRUE)

}
