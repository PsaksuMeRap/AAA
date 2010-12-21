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

test.moneyToString <- function() {
	currency = "CHF"
	amount = 1105.3
	
	money <- toMoney(amount,currency)
	
	checkEquals(money$toString(),paste(currency, "1'105.30"))
}

test.sumMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- toMoney(100,"CHF")
	money2 <- toMoney(20.55,"CHF")
	money3 <- toMoney(200,"USD") # exchange rate USD-CHF: 0.9627
	money4 <- toMoney(100,"EUR") # exchange rate EUR-CHF: 1.33853808
	
	money1$sum(money2);	checkEquals(money1,toMoney(120.55,"CHF"))
	money1$sum(money3); checkEquals(money1,toMoney(100+200*0.9627,"CHF"))
	money3$sum(money1); checkEquals(money3,toMoney(200+100/0.9627,"USD"))
	money4$sum(money3); checkEquals(money4,toMoney(100+200*0.9627/1.33853808,"EUR"))
	
	repositories$exchangeRates <- repository
}

test.divideMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- toMoney(100,"CHF")
	money2 <- toMoney( 25,"CHF")
	money3 <- toMoney(200,"USD") # exchange rate USD-CHF: 0.9627
	money4 <- toMoney(100,"EUR") # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1$divide(money2),4)
	checkEquals(money1$divide(money3),100/(200*0.9627))
	checkEquals(money3$divide(money4),200/(100*1.33853808/0.9627))
	
	repositories$exchangeRates <- repository
}
