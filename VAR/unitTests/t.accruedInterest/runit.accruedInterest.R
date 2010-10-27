# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_accruedInterest <- function() {
	money <- toMoney(amount=10.2,currency="USD")
	
	accruedInterest <- create_accruedInterest(money,"2000-02-21")
	checkEquals(class(accruedInterest),"accruedInterest")
	checkEquals(accruedInterest$money$amount,10.2)
	checkEquals(accruedInterest$money$currency,"USD")
	checkEquals(accruedInterest$date,"2000-02-21")
}
