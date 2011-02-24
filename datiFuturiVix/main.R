# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RUnit")
home <- "/home/claudio/eclipse/AAA/datiFuturiVix/"
setwd(home)

stringsAsFactors = FALSE

source("./lib/library.R")


importerVix <- create_importer()
repository <- importerVix$createRepository()
rm(importerVix)

importerVixFutures <- create_importerVixFutures()
contracts <- importerVixFutures$extractAllContracts()
rm(importerVixFutures)

# determina le lastTradeDates

settlementDates <- extractLists(contracts,fieldName="settlementDate")
lastTradeDates  <- extractLists(contracts,fieldName="lastTradeDate")
settlementPrices <- sapply(contracts,extractPriceAtSettlementOrLastTradeDate,dateType="settlementDate")
lastTradePrices <- sapply(contracts,extractPriceAtSettlementOrLastTradeDate,dateType="lastTradeDate")




