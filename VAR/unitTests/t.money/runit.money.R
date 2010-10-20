# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./lib/money.R")

test.createMoney <- function() {
	currency = "CHF"
	amount = 105.3
	
	money <- toMoney(amount,currency)
	
	checkEquals(class(money),"money")
	checkEquals(money$amount,105.3)
	checkEquals(money$currency,"CHF")

}
