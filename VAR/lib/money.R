# TODO: Add comment
# 
# Author: claudio
###############################################################################


toMoney <- function(amount,currency) {
	# create an object of class money
	money <- list(amount=amount,currency=currency)
	class(money) <- "money"

	return(money)
}

