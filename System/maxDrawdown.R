# TODO: Add comment
# 
# Author: claudio
###############################################################################


cumMaxDrawdown <- function (priceOrWelth)
{
	cummax(1 - priceOrWelth / cummax(priceOrWelth))
}

maxDrawdown <- function (priceOrWelth)
{
	max(1 - priceOrWelth / cummax(priceOrWelth))
}

dati <- read.csv("globalEquity.csv")
prezzi <- dati[(nrow(dati)-246):nrow(dati),c("TRADE_DATE","Close")]

mdd <- maxDrawdown(prezzi[,"Close"])
mddc <- cumMaxDrawdown(prezzi[,"Close"])