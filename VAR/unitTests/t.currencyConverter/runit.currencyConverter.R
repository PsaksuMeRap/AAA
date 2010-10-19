# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.exchangeAmountFromTo <- function() {
	
	rates_tmp <- repositories$ExchangeRates
	
	rates <- create_repositoryExchangeRates()$rates
	
	# convert 100 EUR to CHF
	currencyTo <- "CHF"
	amountConverted <- 100.0 * 1.33853808
	result <- convertTo(list(amount=100,currency="EUR"),currencyTo)
	checkEquals(result$amount,amountConverted)
	checkEquals(result$currency,currencyTo)
	
	# convert 100 CHF to USD
	currencyTo <- "USD"
	amountConverted <- 100.0 / 0.9627
	result <- convertTo(list(amount=100,currency="CHF"),currencyTo)
	checkEquals(result$amount,amountConverted)
	checkEquals(result$currency,currencyTo)
	
	# convert 100 EUR to USD
	currencyTo <- "USD"
	amountConverted <- 100.0 * 1.33853808 / 0.9627
	result <- convertTo(list(amount=100,currency="EUR"),currencyTo)
	checkEquals(result$amount,amountConverted)
	checkEquals(result$currency,currencyTo)
	
	eval(expression(exchangeRates <- rates_tmp),env=repositories)
}



