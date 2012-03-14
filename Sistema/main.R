# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")

if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/Sistema"
} else {
	home <- "/home/claudio/workspace/AAA/Sistema/"
}

setwd(home)

stringsAsFactors = FALSE

source("./lib/library.R")

# initialize the repository environment variable
repositories <- new.env()

# create the instrument repository 
source("./unitTests/utilities/allocateTestRepositories.R")  
allocateTestRepositories("instruments")
allocateTestRepositories("politicaInvestimento")


source("./unitTests/utilities/createOriginData.R")
dati <- new("AyrtonPositions",createOriginData())

politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df
portfolios <- portfoliosFactory(dati,politicaInvestimento.df)
