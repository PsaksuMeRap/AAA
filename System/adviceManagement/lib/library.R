# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./adviceManagement/lib/archive.R")
source("./adviceManagement/lib/logger.R") 
source("./adviceManagement/lib/lock_unlock.R")
source("./adviceManagement/lib/file.move.R")
source("./adviceManagement/lib/zipResults.R")

source("./adviceManagement/lib/methods/repositories/repositoryExchangeRates.R") 
source("./adviceManagement/lib/methods/repositories/repositoryInstruments.R") 
source("./adviceManagement/lib/methods/repositories/repositoryPoliticaInvestimento.R") 

source("./adviceManagement/lib/methods/portfolio/loadLastPortfolio.R")
source("./adviceManagement/lib/methods/tradeToSec_Pos_Por/tradeToSecurityFactory.R")
source("./adviceManagement/lib/methods/tradeToSec_Pos_Por/tradeToPositionFactory.R")
source("./adviceManagement/lib/methods/tradeToSec_Pos_Por/tradeToPositionsFactory.R")
source("./adviceManagement/lib/methods/tradeToSec_Pos_Por/tradesToPositionsFactory.R")
source("./adviceManagement/lib/saveLastObject.R")
source("./adviceManagement/lib/classDefinitions/Mail/Mail.R")

source("./adviceManagement/lib/classDefinitions/BloombergData/BloombergData.R")
source("./adviceManagement/lib/classDefinitions/BloombergRequestHandler/BloombergRequestHandler.R")
source("./adviceManagement/lib/classDefinitions/Advisor/Advisor.R")
source("./adviceManagement/lib/classDefinitions/Advisors/Advisors.R")
source("./adviceManagement/lib/classDefinitions/Trade/Trade.R")
source("./adviceManagement/lib/classDefinitions/Trades/Trades.R")
source("./adviceManagement/lib/classDefinitions/IdBloomberg/IdBloomberg.R")

source("./adviceManagement/lib/tradeFactory.R")
source("./adviceManagement/lib/tradesFactory.R")
source("./adviceManagement/lib/classDefinitions/MessageFileName/MessageFileName.R")
source("./adviceManagement/lib/messageFileNameFactory.R") 
source("./adviceManagement/lib/classDefinitions/Message/Message.R")
source("./adviceManagement/lib/messageFactory.R") 
source("./adviceManagement/lib/get_PID.R") 
source("./adviceManagement/lib/classDefinitions/PostOffice/PostOffice.R")
source("./adviceManagement/lib/classDefinitions/MailBox/MailBox.R") 
source("./adviceManagement/lib/methods/mainMessageProcessing/mainMessageProcessing_1_newAdviceNoLock.R") 
source("./adviceManagement/lib/methods/mainMessageProcessing/mainMessageProcessing_1_newAdviceWithLock.R")
source("./adviceManagement/lib/methods/mainMessageProcessing/mainMessageProcessing.R")
source("./adviceManagement/lib/methods/mainMessageProcessing/mainMessageProcessing_1_newAdvice.R")
source("./adviceManagement/lib/methods/mainMessageProcessing/mainMessageProcessing_2_PreComplianceResult.R")
source("./adviceManagement/lib/methods/mainMessageProcessing/mainMessageProcessing_3_confirmation.R")
source("./adviceManagement/lib/methods/mainMessageProcessing/mainMessageProcessing_4_PostComplianceResult.R")

