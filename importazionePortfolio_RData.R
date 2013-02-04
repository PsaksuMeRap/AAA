# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")

if(.Platform$OS.type=="windows") {
	sourceCodeDir <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
	homeDir <- sourceCodeDir
} else {
	sourceCodeDir <- "/home/claudio/workspace/AAA/System/"
	homeDir <- sourceCodeDir
}

setwd(sourceCodeDir)

stringsAsFactors = FALSE
repositories <- new.env()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")
## -- fine setup


pf <- loadPortfolio(dir="/home/claudio/Desktop",portfolioId="multistrategy")

for (i in pf) {
	print(paste(i@security@name,i@security@id@idAAA))
}
