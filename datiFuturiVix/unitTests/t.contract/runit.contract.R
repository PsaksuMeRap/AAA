# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_contract <- function() {
	
	contract <- create_contract()
	
	checkEquals(class(contract),"Future")
	checkEquals(contract$name,NA_character_)
	checkEquals(contract$settlementDate,NA_character_)
	checkEquals(contract$lastTradeDate,NA_character_)
	checkEquals(contract$data,NA)	
}
