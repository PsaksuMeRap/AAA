# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")

home <- "/home/claudio/workspace/AAA/Sistema/"
# home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/Sistema"
setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()

source("./lib/library.R")

source("./unitTests/utilities/createOriginData.R")
dati <- new("AyrtonPositions",createOriginData())

# create the instrument repository 
source("./unitTests/utilities/allocateTestRepositories.R")  
allocateTestRepositories("instruments")
allocateTestRepositories("politicaInvestimento")
politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df
portfolios <- portfoliosFactory(dati,politicaInvestimento.df)

