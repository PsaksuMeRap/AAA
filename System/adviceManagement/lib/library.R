# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./adviceManagement/lib/archive.R")
source("./adviceManagement/lib/logger.R") 
source("./adviceManagement/lib/lock_unlock.R")
source("./adviceManagement/lib/file.move.R") 
source("./adviceManagement/lib/methods/portfolio/loadLastPortfolio.R")
source("./adviceManagement/lib/methods/tradeToPosition/tradeToSecurityFactoryStep1.R")
source("./adviceManagement/lib/saveLastObject.R")
source("./adviceManagement/lib/classDefinitions/Mail/Mail.R")
source("./adviceManagement/lib/isLockOnNewAdvice.R")

source("./adviceManagement/lib/classDefinitions/BloombergData/BloombergData.R")
source("./adviceManagement/lib/classDefinitions/BloombergRequestHandler/BloombergRequestHandler.R")
source("./adviceManagement/lib/classDefinitions/Advisor/Advisor.R")
source("./adviceManagement/lib/classDefinitions/Advisors/Advisors.R")
source("./adviceManagement/lib/classDefinitions/Trade/Trade.R")
source("./adviceManagement/lib/classDefinitions/Trades/Trades.R")

source("./adviceManagement/lib/tradeFactory.R")
source("./adviceManagement/lib/tradesFactory.R")
source("./adviceManagement/lib/classDefinitions/Message/Message.R")
source("./adviceManagement/lib/messageFactory.R") 
source("./adviceManagement/lib/get_PID.R") 
source("./adviceManagement/lib/classDefinitions/PostOffice/PostOffice.R") 
source("./adviceManagement/lib/noLockOnNewAdvice.R") 
source("./adviceManagement/lib/isLockOnNewAdvice.R")
source("./adviceManagement/lib/methods/messageProcessing.R")
