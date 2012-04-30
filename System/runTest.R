# TODO: Add comment
# 
# Author: claudio
###############################################################################

# import the exchange rates
if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/base/"
} else {
	home <- "/home/claudio/workspace/AAA/System/base/"
}

setwd(home)

source("./unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

# set the working directory to System
if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
} else {
	home <- "/home/claudio/workspace/AAA/System/"
}

setwd(home)

# execute the test of the base
source("./base/unitTests/runTest.R")

# the ayrton test suites 
source("./ayrton/unitTests/runTest.R")

# the riskman test suites 
source("./riskman/unitTests/runTest.R")




