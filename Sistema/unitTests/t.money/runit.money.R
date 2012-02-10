# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./lib/classDefinitions/Money/Money.R")

test.createMoney <- function() {
	currency = new("Currency","CHF")
	amount = 105.3
	
	money <- new("Money",amount=amount,currency=currency)
	
	checkEquals(class(money)[1],"Money")
	checkEquals(money@amount,105.3)
	checkEquals(money@currency,currency)

}


test.moneyToString <- function() {
	currency = new("Currency","CHF")
	amount = 1105.3
	
	money <- new("Money",amount=amount,currency=currency)
	
	checkEquals(toString(money),paste(currency, "1'105.30"))
}

test.sumMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=100,currency=new("Currency","CHF"))
	money2 <- new("Money",amount=20.55,currency=new("Currency","CHF"))
	money3 <- new("Money",amount=200,currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=100,currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	money <- sum(money1,money2); checkEquals(money,new("Money",amount=120.55,currency=new("Currency","CHF")))
	money <- sum(money1,money3); checkEquals(money,new("Money",amount=100+200*0.9627,currency=new("Currency","CHF")))
	money <- sum(money3,money1); checkEquals(money,new("Money",amount=200+100/0.9627,currency=new("Currency","USD")))
	money <- sum(money4,money3); checkEquals(money,new("Money",amount=100+200*0.9627/1.33853808,currency=new("Currency","EUR")))
	
	repositories$exchangeRates <- repository
}

test.divideMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=100,currency=new("Currency","CHF"))
	money2 <- new("Money",amount=25, currency=new("Currency","CHF"))
	money3 <- new("Money",amount=200,currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=100,currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(divide(money1,money2),4)
	checkEquals(divide(money1,money3),100/(200*0.9627))
	checkEquals(divide(money3,money4),200/(100*1.33853808/0.9627))
	
	repositories$exchangeRates <- repository
}
