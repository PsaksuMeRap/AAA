# TODO: Add comment
# 
# Author: Claudio
###############################################################################

library("RODBC")
library("tcltk")

if(.Platform$OS.type=="windows") {
	homeDir <- "C:/riskman"
	if (!exists("sourceCodeDir")) sourceCodeDir <- getwd() else setwd(sourceCodeDir)
} else {
	homeDir <- "/home/claudio/riskman"
	if (!exists("sourceCodeDir")) sourceCodeDir <- getwd() else setwd(sourceCodeDir)

}


stringsAsFactors = FALSE
repositories <- new.env()

# copy the data directory
fileFrom <- file.path(sourceCodeDir,"applicationData","data") 
fileTo <- file.path(homeDir)
ok <- file.copy(from=fileFrom,to=fileTo,recursive = TRUE)

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")
source("./adviceManagement/lib/library.R")
## -- fine setup


## -- inizio procedura salvataggio portafogli - parte generale
source("./odbc/connessioni.R")


## salva il nuovo DBEquity nella cartella data prima dell'importazione
# salva il DBEquities
repositoryDBEquities <- create_DBEquities()
directory <- file.path(homeDir,"data","DBEquities")
saveLastObject(repositoryDBEquities[["DBEquities.df"]],fileName="DBEquities.RData",directory)

## salva il nuovo repositoryExchangeRates nela cartella data prima dell'importazione
repositoryExchangeRates <- create_repositoryExchangeRates()
directory <- file.path(homeDir,"data","exchangeRates")
saveLastObject(repositoryExchangeRates,fileName="exchangeRates.RData",directory)


dati <- importDBPortfolioGenerale()

repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$exchangeRates <- create_repositoryExchangeRates()

setwd(homeDir)
## -- fine procedura salvataggio portafogli - parte generale


mappingPippoPortfolio <- c(
		pippo53="globalEquity",
		pippo76="fixedIncome",
		pippo210="globalEconomy",
		pippo100="asymmetricEquity",
		pippo101="multistrategy"
)

## caso con data storica
if (FALSE) {
	date <- c("2012-04-13")
	# controlla che la data sia dopo il "2011-05-31" altrimenti togli il fondo globalEconomy
	if (as.Date(date) > as.Date("2011-05-31")) fundsOwners <- c("pippo53","pippo76","pippo210","pippo100","pippo101") else fundsOwners <- c("pippo53","pippo76","pippo100","pippo101")
	repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)
	
	# importa i dati dalla tabella del DB
	dati <- importDBPortfolioGeneraleByDate(date)
	
	# filtra solo i fondi
	fundPortfolios <- filterLists(dati,"Cliente",value=fundsOwners)
	
	# crea i portafogli
	fundPortfolios <- portfoliosFactory(fundPortfolios)
	
} else {
	# caso data attuale
	fundsOwners <- c("pippo53","pippo76","pippo210","pippo100","pippo101")
	fundPortfolios <- filterLists(dati,"Cliente",value=fundsOwners)	
	fundPortfolios <- portfoliosFactory(fundPortfolios)
}

# salva i portafogli in homeDir
for (portfolio in fundPortfolios) {
	portfolioName <- mappingPippoPortfolio[[portfolio@owner]]
	directory <- file.path(homeDir,"dataNew","portfolios",portfolioName)
	if (!file.exists(directory)) dir.create(directory,recursive=TRUE)
	directory <- file.path(homeDir,"dataNew","portfolios")
	savePortfolio(portfolio,portfolioSubDirectory=portfolioName,directory=directory)
}

# salva repositoryDBEquities in dataNew
directory <- file.path(homeDir,"dataNew","DBEquities")
if (!file.exists(directory)) dir.create(directory,recursive=TRUE) 
saveLastObject(repositoryDBEquities[["DBEquities.df"]],fileName="DBEquities.RData",directory)

# salva repositoryExchangeRates in dataNew
directory <- file.path(homeDir,"dataNew","exchangeRates")
if (!file.exists(directory)) dir.create(directory,recursive=TRUE) 
saveLastObject(repositoryExchangeRates,fileName="exchangeRates.RData",directory)

# copy the script file
from <- file.path(sourceCodeDir,"Sincronizza.R")
to <- file.path(homeDir,"Sincronizza.R")
ok <- file.copy(from,to)

# zip the portfolio's directories
zipFullFileName <- file.path(homeDir,"portfolios.zip")
files <- file.path(homeDir,"dataNew")
files <- c(files,file.path(homeDir,"Sincronizza.R"))
ok <- zip(zipFullFileName, files, flags = "-r9X")
## -- terminato salvataggio portafogli




## -- funzioni di utilità
whoisP <- function(names) {
	df <- read.csv("/home/claudio/workspace/Produzione_new/associazionePippo.csv",header=TRUE,sep=",",
			as.is=TRUE)
	
	getName <- function(name,df) {isOk <- df[,1]==name;return(df[isOk,2])}
	pippoNames <- sapply(names,getName,df)
	return(paste(names,"->",pippoNames))
}
