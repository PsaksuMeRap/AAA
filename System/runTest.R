# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

stringsAsFactors = FALSE
repositories <- new.env()

source("./base/lib/library.R")
systemOptions[["sourceCodeDir"]] <- getwd()

## -- fine setup 


# import the exchange rates
# setwd(file.path(systemOptions[["sourceCodeDir"]],"base"))

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

# setwd(systemOptions[["sourceCodeDir"]])

# execute the test of the base
source("./base/unitTests/runTest.R")

# the ayrton test suites 
source("./ayrton/unitTests/runTest.R")

# the riskman test suites 
source("./riskman/unitTests/runTest.R")
