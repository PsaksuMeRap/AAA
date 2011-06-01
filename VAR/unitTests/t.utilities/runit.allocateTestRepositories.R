# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldAllocateTestInterestRates <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")

	allocateTestRepositories("interestRates")
	
	checkEquals(repositories$interestRates$getMonthTicker(12),"1Y")
	checkEquals(repositories$interestRates$getMonthTicker(1),"1M")
	checkEquals(repositories$interestRates$getMonthTicker(0.25),"1W")
	
	deallocateTestRepositories("interestRates")
}


test.shouldAllocateTestExchangeRates <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("exchangeRates")
	
	checkEquals(repositories$exchangeRates$rates[["ATS"]],0.0973132896811843)
	checkEquals(repositories$exchangeRates$rates[["EUR"]],1.33853808)
	
	deallocateTestRepositories("exchangeRates")
}



test.shouldAllocateTestEquities <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("equities")
	
	checkEquals(repositories$equities$equities.df[3,"equity"],"AVENTIS")
	checkEquals(repositories$equities$equities.df[9,"ticker"],"MIRZn.S")
	
	deallocateTestRepositories("equities")
}

test.shouldAllocateTestInstruments <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("instruments")
	
	checkEquals(repositories$instruments$getInstrumentName(1),"equity")
	checkEquals(repositories$instruments$getId("Obbligazioni_convertibili"),11)
	
	deallocateTestRepositories("instruments")
}

test.shouldAllocateTestPoliticaInvestimento <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("politicaInvestimento")
	
	owner <- "pippo47"
	isDesiredOwner <- repositories$politicaInvestimento$politicaInvestimento.df[,"ID"] == owner
	refCurrency <- repositories$politicaInvestimento$politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"]	
	checkEquals(refCurrency,"USD")
	
	owner <- "pippo165"
	isDesiredOwner <- repositories$politicaInvestimento$politicaInvestimento.df[,"ID"] == owner
	refCurrency <- repositories$politicaInvestimento$politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"]
	checkEquals(refCurrency,"EUR")
	
	owner <- "pippo66"
	isDesiredOwner <- repositories$politicaInvestimento$politicaInvestimento.df[,"ID"] == owner
	refCurrency <- repositories$politicaInvestimento$politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"]	
	checkEquals(refCurrency,"CHF")
	
	deallocateTestRepositories("politicaInvestimento")
}


test.shouldAllocateTestFixedIncome <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	allocateTestRepositories("fixedIncome")
	nomi <- c("ID_strumento","ID","NomeDebitoreRedditoFisso","ProvenienzaDebitoreCH","NumeroValore","RatingRedditoFisso","Reddito","Moneta","ScadenzaRedditoFisso","DataLiberazione","BaseInteresseRedditoFisso","FrequenzaRedditoFisso","NumeroProspetto","TransazioneInSvizzera","Settore","ISIN","Data_iniziale_1","Data_finale_1","Data_penultima_cedola","Fixed_NonFixed_per_Act365","rowguid","Date Moody","Rating Moody","Date SP","Rating SP")
	origin_linea1 <- list(2,269,"Switzerland 04","0","015759","AAA",0.0649999976158142,"CHF","2004-04-10 00:00:00.000",NA,"30/360","annuale",364,"0","Government",NA,NA,NA,NA,NA,"50C429E9-619D-482F-819D-DE92985AB19A",NA,NA,NA,NA)
	origin_linea12 <- list(2,915,"Volvo Treasury AB","0","1004344","NR",0.0512499995529652,"EUR","2004-10-12 00:00:00.000",NA,"30/360","annuale",NA,"0",NA,NA,NA,NA,NA,NA,"8341EE38-3C4E-46CB-9765-66B7CCBA3E91",NA,NA,NA,NA)
	names(origin_linea1) <- nomi
	names(origin_linea12) <- nomi
	
	linea1 <- as.list(repositories$fixedIncome$fixedIncome.df[1,])
	linea12 <- as.list(repositories$fixedIncome$fixedIncome.df[12,])
	
	checkEquals(origin_linea1["NomeDebitoreRedditoFisso"],linea1["NomeDebitoreRedditoFisso"])
	checkEquals(origin_linea1["rowguid"],linea1["rowguid"])
	checkEquals(origin_linea12["ID"],linea12["ID"])
	checkEquals(origin_linea12["NumeroValore"],linea12["NumeroValore"])
	
	deallocateTestRepositories("fixedIncome")
}
