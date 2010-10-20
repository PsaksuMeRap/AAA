# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")

toMoney <- function(amount,currency) {
	# create an object of class money
	money <- new.env()
	class(money) <- "money"

	money$amount <- amount
	money$currency <- currency
	
	money$sum <- function(m) {
		# m: a money
		if (m$currency!=money$currency) m <- repositories$exchangeRates$exchange(m,money$currency)
		money$amount <<- money$amount+m$amount
	}
	money$print <- function() {
		print(paste(money$currency,money$amount))
	}
	return(money)
}

