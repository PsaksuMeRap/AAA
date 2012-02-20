# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/lists.R")

source("./lib/repository.R")
source("./lib/importazioneDBPortfolioGenerale.R") 
source("./lib/genericFunctions.R") 

source("./lib/classDefinitions/Amount/Amount.R")
source("./lib/classDefinitions/Currency/Currency.R") 
source("./lib/classDefinitions/Money/Money.R") 
source("./lib/classDefinitions/AyrtonPosition/AyrtonPosition.R")
source("./lib/classDefinitions/AyrtonPositions/AyrtonPositions.R")

source("./lib/classDefinitions/Security/Bond.R")
source("./lib/classDefinitions/Security/Equity.R")
source("./lib/classDefinitions/Security/Fondi_obbligazionari.R")
source("./lib/classDefinitions/Security/Unclassified.R")

source("./lib/classDefinitions/Id/Id.R")
source("./lib/classDefinitions/Position/PositionEquity/PositionEquity.R")
source("./lib/classDefinitions/Position/PositionBond/PositionBond.R")
source("./lib/classDefinitions/Position/PositionBond/AccruedInterest.R")
source("./lib/classDefinitions/Positions/Positions.R")

source("./lib/setMethod/securityFactory/securityFactory.R") 
source("./lib/setMethod/positionFactory/positionFactory.R")
source("./lib/setMethod/positionsFactory/positionsFactory.R")
source("./lib/setMethod/positionsFactory/completeBondPosition.R")
source("./lib/setMethod/positionsFactory/matchToPositionBond.R")
source("./lib/setMethod/positionsFactory/adjustForAccruedInterest.R")