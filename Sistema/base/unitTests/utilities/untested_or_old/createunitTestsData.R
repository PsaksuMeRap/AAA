# TODO: Add comment
# 
# Author: claudio
###############################################################################


createUnitTestsData <- function() {
	instruments <- create_repositoryInstruments()
	write.csv(instruments$instruments.df,"./unitTests/data/tmp/repositoryInstruments.csv",row.names=FALSE)
	save("instruments",file="./unitTests/data/tmp/instrumentsRepo_RData")
	
	equities <- create_repositoryEquities()
	write.csv(equities$equities.df,"./unitTests/data/tmp/repositoryEquities.csv",row.names=FALSE)
	save("equities",file="./unitTests/data/tmp/equitiesRepo_RData")
	
	interestRates <- create_repositoryInterestRates()
	write.csv(interestRates$interestRates.df,"./unitTests/data/tmp/repositoryInterestRates.csv",row.names=FALSE)
	save("interestRates",file="./unitTests/data/tmp/interestRatesRepo_RData")
	
	#discountFactors <- create_repositoryDiscountFactors()
	#write.csv(discountFactors$discountFactors.df,"./unitTests/data/tmp/repositoryDiscountFactors.csv",row.names=FALSE)
	#save("discountFactors",file="./unitTests/data/tmp/discountFactorsRepo_RData")
	
	exchangeRates <- create_repositoryExchangeRates()
	exchangeRates.df <- data.frame(Moneta=names(exchangeRates$rates),CHFPar=exchangeRates$rates)
	write.csv(exchangeRates.df,"./unitTests/data/tmp/repositoryExchangeRates.csv",row.names=FALSE)
	save("exchangeRates",file="./unitTests/data/tmp/exchangeRatesRepo_RData")
	
	fixedIncome <- create_repositoryFixedIncome()
	write.csv(fixedIncome$fixedIncome.df,"./unitTests/data/tmp/repositoryFixedIncome.csv",row.names=FALSE)
	save("fixedIncome",file="./unitTests/data/tmp/fixedIncomeRepo_RData")
	
	politicaInvestimento <- create_repositoryPoliticaInvestimento()
	write.csv(politicaInvestimento$politicaInvestimento.df,"./unitTests/data/tmp/repositoryPoliticaInvestimento.csv",row.names=FALSE)
	save("politicaInvestimento",file="./unitTests/data/tmp/politicaInvestimentoRepo_RData")

	origin.df <- importDBPortfolioGeneraleDataFrame()
	write.csv(origin.df,"./unitTests/data/tmp/origin.csv",row.names=FALSE,na = "")
	save("origin.df",file="./unitTests/data/tmp/originDataFrame_RData")
	
}

