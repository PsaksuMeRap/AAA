test.create_contracts <- function() {
	
	contract1 <- create_contract(name="2011-03",settlementDate="2011-03-15",
			lastTradeDate="2011-03-16")
	contract2 <- create_contract(name="2011-05",settlementDate="2011-05-17",
			lastTradeDate="2011-05-18")
	contract3 <- create_contract(name="2011-04",settlementDate="2011-04-19",
			lastTradeDate="2011-04-20")
	
	contracts <- list(contract1,contract2,contract3)

	orderedContracts <- list(contract1,contract3,contract2)
	class(orderedContracts) <- "vixFutures"

	# ordina i contratti
	settlementDates <- extractFromList(contracts,fieldName="settlementDate")
	orderSettlementDates <- order(as.Date(settlementDates))
	contracts <- contracts[orderSettlementDates]
	class(contracts) <- "vixFutures"
	
	checkEquals(contracts,orderedContracts)
}
