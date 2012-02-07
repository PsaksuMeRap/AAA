# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./lib/classDefinitions/Money/Money.R")

test.createMoney <- function() {
	currency = "CHF"
	amount = 105.3
	
	money <- new("Money",amount=amount,currency=currency)
	
	checkEquals(class(money)[1],"Money")
	checkEquals(money@amount,105.3)
	checkEquals(money@currency,"CHF")

}


test.moneyToString <- function() {
	currency = "CHF"
	amount = 1105.3
	
	money <- new("Money",amount=amount,currency=currency)
	
	checkEquals(toString(money),paste(currency, "1'105.30"))
}

test.sumMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=100,currency="CHF")
	money2 <- new("Money",amount=20.55,currency="CHF")
	money3 <- new("Money",amount=200,currency="USD") # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=100,currency="EUR") # exchange rate EUR-CHF: 1.33853808
	
	money <- sum(money1,money2); checkEquals(money,new("Money",amount=120.55,currency="CHF"))
	money <- sum(money1,money3); checkEquals(money,new("Money",amount=100+200*0.9627,currency="CHF"))
	money <- sum(money3,money1); checkEquals(money,new("Money",amount=200+100/0.9627,currency="USD"))
	money <- sum(money4,money3); checkEquals(money,new("Money",amount=100+200*0.9627/1.33853808,currency="EUR"))
	
	repositories$exchangeRates <- repository
}

test.divideMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=100,currency="CHF")
	money2 <- new("Money",amount=25, currency="CHF")
	money3 <- new("Money",amount=200,currency="USD") # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=100,currency="EUR") # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(divide(money1,money2),4)
	checkEquals(divide(money1,money3),100/(200*0.9627))
	checkEquals(divide(money3,money4),200/(100*1.33853808/0.9627))
	
	repositories$exchangeRates <- repository
}
