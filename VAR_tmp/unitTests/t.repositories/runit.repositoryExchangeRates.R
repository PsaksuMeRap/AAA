# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_repositoryExchangeRates <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/createExchangeRatesVector.R")
	
	rates <- createExchangeRatesVector()
	repositoryExchangeRates <- create_repositoryExchangeRates(rates)
	
	checkEquals(repositoryExchangeRates$rates[["ESP"]],0.00804791304556874)
	checkEquals(repositoryExchangeRates$rates[["CHF"]],1)
}



test.exchange <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/createExchangeRatesVector.R")
	
	rates <- createExchangeRatesVector()
	repo <- create_repositoryExchangeRates(rates)
	
	# convert 100 EUR to CHF
	currencyTo <- "CHF"
	amountConverted <- 100.0 * 1.33853808

	result <- repo$exchange(toMoney(100,"EUR"),currencyTo)

	checkEquals(result$amount,amountConverted)
	checkEquals(result$currency,currencyTo)

	# convert 100 CHF to USD
	currencyTo <- "USD"
	amountConverted <- 100.0 / 0.9627

	result <- repo$exchange(toMoney(100,"CHF"),currencyTo)
	checkEquals(result$amount,amountConverted)
	checkEquals(result$currency,currencyTo)
	
	# convert 100 EUR to USD
	currencyTo <- "USD"
	amountConverted <- 100.0 * 1.33853808 / 0.9627
	result <- repo$exchange(toMoney(100,"EUR"),currencyTo)
	checkEquals(result$amount,amountConverted)
	checkEquals(result$currency,currencyTo)

}