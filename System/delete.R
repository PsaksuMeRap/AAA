# TODO: Add comment
# 
# Author: claudio
###############################################################################


# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

stringsAsFactors = FALSE
repositories <- new.env()
testFramework <- TRUE

if (.Platform$OS.type=="windows") {
	homeDir <- "C:/riskman"
} else {
	homeDir <- "/home/claudio/riskman"
}

sourceCodeDir <- getwd()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")
source("./adviceManagement/lib/library.R")

sys[["sourceCodeDir"]] <- getwd()

rm(homeDir,sourceCodeDir)

source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
testRepository <- createExchangeRatesTestRepository() 
repositories$exchangeRates <- testRepository

# source the test advisors
source("./adviceManagement/unitTests/t.advisors/advisors.R")

## -- fine setup 



dirs=c()
dirs = c(dirs,

#"./adviceManagement/unitTests/t.tradesToPositionsFactory",	
"./adviceManagement/unitTests/t.tradeToPositionFactory"
#"./adviceManagement/unitTests/t.tradeToPositionsFactory",		
#"./adviceManagement/unitTests/t.tradeToSecurityFactory",
#"./adviceManagement/unitTests/t.zipResults"
)


testsuite.lists <- defineTestSuite("Test adviceManagement",dirs = dirs)
testResult <- runTestSuite(testsuite.lists); printTextProtocol(testResult)
warnings()

