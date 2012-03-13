# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./lib/classDefinitions/Money/Money.R")

test.createMoney <- function() {
	currency = new("Currency","CHF")
	amount = new("Amount",105.3)
	
	money <- new("Money",amount=amount,currency=currency)
	
	checkEquals(class(money)[1],"Money")
	checkEquals(money@amount,new("Amount",105.3))
	checkEquals(money@currency,currency)

}


test.moneyToString <- function() {
	currency = new("Currency","CHF")
	amount = new("Amount",1105.3)
	
	money <- new("Money",amount=amount,currency=currency)
	
	checkEquals(toString(money),paste(currency, "1'105.30"))
}

test.sumMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",20.55),currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	money <- sum(money1,money2); checkEquals(money,new("Money",amount=new("Amount",120.55),currency=new("Currency","CHF")))
	money <- sum(money1,money3); checkEquals(money,new("Money",amount=new("Amount",100+200*0.9627),currency=new("Currency","CHF")))
	money <- sum(money3,money1); checkEquals(money,new("Money",amount=new("Amount",200+100/0.9627),currency=new("Currency","USD")))
	money <- sum(money4,money3); checkEquals(money,new("Money",amount=new("Amount",100+200*0.9627/1.33853808),currency=new("Currency","EUR")))
	
	repositories$exchangeRates <- repository
}

test.divideMoney <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",25), currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1/money2,4)
	checkEquals(money1/money3,100/(200*0.9627))
	checkEquals(money3/money4,200/(100*1.33853808/0.9627))
	
	repositories$exchangeRates <- repository
}

test.toMoneyFromAmountAndCurrency <- function() {

	amount=new("Amount",4.5)
	currency=new("Currency","CHF")
	a <- toMoney(amount=amount,currency=currency)
	checkEquals(a,new("Money",amount=amount,currency=currency))
}

test.toMoneyFromNumericAndCurrency <- function() {
	
	should <- new("Money",amount=new("Amount",40.5),
			currency=new("Currency","CHF"))
	amount=40.5
	currency=new("Currency","CHF")
	a <- toMoney(amount=amount,currency=currency)
	checkEquals(a,should)
}

test.toMoneyFromAmountAndString <- function() {
	
	should <- new("Money",amount=new("Amount",40.5),
			currency=new("Currency","CHF"))
	amount=new("Amount",40.5)
	currency="CHF"
	a <- toMoney(amount=amount,currency=currency)
	checkEquals(a,should)
}

test.toMoneyFromAmountAndString <- function() {
	
	should <- new("Money",amount=new("Amount",40.5),
			currency=new("Currency","CHF"))
	amount=40.5
	currency="CHF"
	a <- toMoney(amount=amount,currency=currency)
	checkEquals(a,should)
}

test.shouldBeGreaterThan <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",20.55),currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1>money2,TRUE)
	checkEquals(money2>money1,FALSE)
	checkEquals(money1>money1,FALSE)
	checkEquals(money1>money3,FALSE)
	checkEquals(money4>money1,TRUE)
	checkEquals(money3>money4,TRUE)
	checkEquals(money4>money4,FALSE)
	
	repositories$exchangeRates <- repository
}

test.shouldBeGreaterOrEqualThan <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",20.55),currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1>=money2,TRUE)
	checkEquals(money2>=money1,FALSE)
	checkEquals(money1>=money1,TRUE)
	checkEquals(money1>=money3,FALSE)
	checkEquals(money4>=money1,TRUE)
	checkEquals(money3>=money4,TRUE)
	checkEquals(money4>=money4,TRUE)
	
	repositories$exchangeRates <- repository
}

test.shouldBeLessThan <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",20.55),currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1<money2,FALSE)
	checkEquals(money2<money1,TRUE)
	checkEquals(money1<money1,FALSE)
	checkEquals(money1<money3,TRUE)
	checkEquals(money4<money1,FALSE)
	checkEquals(money3<money4,FALSE)
	checkEquals(money4<money4,FALSE)
	
	repositories$exchangeRates <- repository
}

test.shouldBeLessOrEqualThan <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",20.55),currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1<=money2,FALSE)
	checkEquals(money2<=money1,TRUE)
	checkEquals(money1<=money1,TRUE)
	checkEquals(money1<=money3,TRUE)
	checkEquals(money4<=money1,FALSE)
	checkEquals(money3<=money4,FALSE)
	checkEquals(money4<=money4,TRUE)
	
	repositories$exchangeRates <- repository
}

test.shouldBeDifferentFrom <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",20.55),currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1!=money2,TRUE)
	checkEquals(money2!=money1,TRUE)
	checkEquals(money1!=money1,FALSE)
	checkEquals(money1!=money3,TRUE)
	checkEquals(money4!=money1,TRUE)
	checkEquals(money3!=money4,TRUE)
	checkEquals(money4!=money4,FALSE)
	
	repositories$exchangeRates <- repository
}

test.shouldBeEqualTo <- function() {
	
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	money1 <- new("Money",amount=new("Amount",100),currency=new("Currency","CHF"))
	money2 <- new("Money",amount=new("Amount",20.55),currency=new("Currency","CHF"))
	money3 <- new("Money",amount=new("Amount",200),currency=new("Currency","USD")) # exchange rate USD-CHF: 0.9627
	money4 <- new("Money",amount=new("Amount",100),currency=new("Currency","EUR")) # exchange rate EUR-CHF: 1.33853808
	
	checkEquals(money1==money2,FALSE)
	checkEquals(money2==money1,FALSE)
	checkEquals(money1==money1,TRUE)
	checkEquals(money1==money3,FALSE)
	checkEquals(money4==money1,FALSE)
	checkEquals(money3==money4,FALSE)
	checkEquals(money4==money4,TRUE)
	
	repositories$exchangeRates <- repository
}