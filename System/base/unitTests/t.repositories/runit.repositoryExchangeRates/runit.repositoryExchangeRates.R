# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_repositoryExchangeRates <- function() {
	source("./base/lib/repository.R")
	source("./base/unitTests/utilities/createExchangeRatesVector.R")
	
	rates <- createExchangeRatesVector()
	repositoryExchangeRates <- create_testRepositoryExchangeRates(rates)
	
	checkEquals(repositoryExchangeRates$rates[["ESP"]],0.00804791304556874)
	checkEquals(repositoryExchangeRates$rates[["CHF"]],1)
}



test.exchange <- function() {
	source("./base/lib/repository.R")
	source("./base/unitTests/utilities/createExchangeRatesVector.R")
	
	rates <- createExchangeRatesVector()
	repo <- create_testRepositoryExchangeRates(rates)
	
	# convert 100 EUR to CHF
	currencyTo <- new("Currency","CHF")
	amountConverted <- 100.0 * 1.33853808

	result <- repo$exchange(toMoney(100,new("Currency","EUR")),currencyTo)

	checkEquals(result@amount,new("Amount",amountConverted))
	checkEquals(result@currency,currencyTo)

	# convert 100 CHF to USD
	currencyTo <- new("Currency","USD")
	amountConverted <- 100.0 / 0.9627

	result <- repo$exchange(toMoney(100,new("Currency","CHF")),currencyTo)
	checkEquals(result@amount,new("Amount",amountConverted))
	checkEquals(result@currency,currencyTo)
	
	# convert 100 EUR to USD
	currencyTo <- new("Currency","USD")
	amountConverted <- 100.0 * 1.33853808 / 0.9627
	result <- repo$exchange(toMoney(100,new("Currency","EUR")),currencyTo)
	checkEquals(result@amount,new("Amount",amountConverted))
	checkEquals(result@currency,currencyTo)

}

test.update <- function() {
	source("./base/lib/repository.R")
	source("./base/unitTests/utilities/createExchangeRatesVector.R")
	
	rates <- createExchangeRatesVector()
	repo <- create_testRepositoryExchangeRates(rates)
	
	# create values vector
	Sys.sleep(1)
	
	values <- c(1.05,0.80)
	names(values) <- c("EUR","USD")
	updateDateTime <- c(Sys.time(),Sys.time())
	Sys.sleep(1)
	updateDateTime[2] <- Sys.time()
	
	ZAR <- repo$rates[["ZAR"]]
	
	names(updateDateTime) <- names(values)

	result <- repo$update(values,updateDateTime)

	checkEquals(result,TRUE)	
	checkEquals(repo$rates[["EUR"]],1.05)
	checkEquals(repo$rates[["USD"]],0.80)
	checkEquals(repo$rates[["ZAR"]],ZAR)

}

test.shouldComputePPMF <- function() {
	
	# check the idrchf exchange rate
	idrAmount <- 1
	idrchf <- 1.0135
	chfAmount <- idrAmount*idrchf*repositories$exchangeRates$getPricePositionMultFactor("IDRCHF")
	
	# the data has been checked from www.ex.com
	should.ex.com <- 0.00010135
	
	checkEquals(chfAmount,should.ex.com)
	
	# check the nokchf exchange rate
	nokAmount <- 1
	nokchf <- 16.3488
	chfAmount <- nokAmount*nokchf*repositories$exchangeRates$getPricePositionMultFactor("NOKCHF")
	
	# the data has been checked from www.ex.com
	should.ex.com <- 0.163488
	checkEquals(chfAmount,should.ex.com)
	
	# check the nokidr exchange rate
	nokAmount <- 1
	nokchf <- 16.3488
	idrchf <- 1.0135
	
	nokidr <- nokchf/idrchf 
	idrAmount <- nokAmount*nokidr*repositories$exchangeRates$getPricePositionMultFactor("NOKIDR")
	
	# the data has been checked from www.ex.com
	should.ex.com <- 1613.103
	checkEquals(idrAmount,should.ex.com)
	
	# check the chfsek exchange rate
	chfAmount <- 1
	jpychf <- 1.2129
	chfjpy <- 1/jpychf
	jpyAmount <- chfAmount*chfjpy*repositories$exchangeRates$getPricePositionMultFactor("CHFJPY")
	
	# the data has been checked from www.ex.com
	should.ex.com <- 82.447
	checkEquals(round(jpyAmount,3),should.ex.com)
}

