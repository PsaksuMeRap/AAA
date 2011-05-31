# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")

toMoney <- function(amount,currency) {
	# create an object of class money
	money <- new.env()
	class(money) <- "money"

	money$amount <- as.numeric(amount)
	money$currency <- currency
	
	money$sum <- function(m) {
		# m: a money
		if (m$currency!=money$currency) m <- repositories$exchangeRates$exchange(m,money$currency)
		money$amount <<- money$amount+m$amount
	}
	
	money$divide <- function(m) {
		# m: a money 
		if (class(m)=="money") {
			if (m$currency!=money$currency) m <- repositories$exchangeRates$exchange(m,money$currency)
			return(money$amount / m$amount)
		}
		stop("Error: invalid money argument in money$divide method!")
	}
	
	money$toString <- function() {
		getAmountWidth <- function(amount) {
			# questa funzione calcola il numero di caratteri da usare per
			# la formattazione dell'output dell'importo
			nbChar <- nchar(as.character(floor(abs(amount))))
			width <- nbChar %/% 3
			width <- nbChar + width + 3
			if (amount < 0) width <- width + 1 
			
			return(width)
		}
	
		width <- getAmountWidth(money$amount)
		number <- formatC(money$amount,width=width,big.mark = "'",
				decimal.mark = ".",format="f",digits=2)
		string = paste(money$currency,number)
		return(string)
	}
	
	money$print <- function() {
		print(money$toString())
	}
	return(money)
}

