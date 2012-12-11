# TODO: Add comment
# 
# Author: claudio
###############################################################################
# TODO: Add comment
# 
# Author: Claudio
###############################################################################

library("RODBC")

sourceCodeDir <- "/home/claudio/workspace/AAA/System/"
homeDir <- sourceCodeDir


setwd(sourceCodeDir)

stringsAsFactors = FALSE
repositories <- new.env()

source("./odbc/connessioni.R")

connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)

## extract fixed income
query = paste("SELECT * FROM [Prezzi storici reddito fisso].dbo.PrezziStorici",
		"WHERE NumeroValore = 'LU0810451608'")

fixedIncomeData.df <- sqlQuery(connection,query,as.is=TRUE)
fixedIncomeData.df[["Price"]] <- fixedIncomeData.df[["Price"]] / 100

write.csv(fixedIncomeData.df,file="fixedIncome.csv")

## extract global equity
query = paste("SELECT * FROM [Prezzi storici azioni].dbo.Fondi",
		"WHERE Ticker = 'LU0810451434'")

globalEquityData.df <- sqlQuery(connection,query,as.is=TRUE)

write.csv(globalEquityData.df ,file="globalEquity.csv")
