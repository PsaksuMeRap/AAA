# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/lists.R")
source("./lib/riskmanOptions.R")

source("./lib/repository.R")
source("./lib/importazioneDBPortfolioGenerale.R") 
source("./lib/genericFunctions.R")


source("./lib/classDefinitions/Amount/Amount.R")
source("./lib/classDefinitions/Currency/Currency.R") 
source("./lib/classDefinitions/Money/Money.R") 
source("./lib/classDefinitions/AyrtonPosition/AyrtonPosition.R")
source("./lib/classDefinitions/AyrtonPositions/AyrtonPositions.R")
source("./lib/classDefinitions/Constraint/Constraint.R") 
source("./lib/classDefinitions/Strings/CheckString.R")
source("./lib/classDefinitions/Strings/CheckStringParsed.R")
source("./lib/classDefinitions/Strings/ConstraintString.R")
source("./lib/classDefinitions/Strings/DirectiveString.R")
source("./lib/classDefinitions/Strings/FactorString.R")
source("./lib/classDefinitions/Strings/FactorStrings.R")
source("./lib/classDefinitions/Strings/SelectionString.R")
source("./lib/classDefinitions/Strings/FactorStringParsed.R")


source("./lib/classDefinitions/Security/Bond.R")
source("./lib/classDefinitions/Security/Equity.R")
source("./lib/classDefinitions/Security/Fondi_obbligazionari.R")
source("./lib/classDefinitions/Security/Fondi_azionari.R")
source("./lib/classDefinitions/Security/Unclassified.R")
source("./lib/classDefinitions/Security/Conto_corrente.R")
source("./lib/classDefinitions/Security/Strutturati_FI.R")
source("./lib/classDefinitions/Security/Strutturati_EQ.R")
source("./lib/classDefinitions/Security/ETF_equity.R")
source("./lib/classDefinitions/Security/Metalli_preziosi.R")
source("./lib/classDefinitions/Security/FX_Forward.R")
source("./lib/classDefinitions/Security/Anticipi_fissi.R")
source("./lib/classDefinitions/Security/Futures_EQ.R")
source("./lib/classDefinitions/Security/Index_certificate.R")
source("./lib/classDefinitions/Security/Opzioni_su_azioni.R")
source("./lib/classDefinitions/Security/Fondi_immobiliari.R")
source("./lib/classDefinitions/Security/Fondi_Hedge.R")
source("./lib/classDefinitions/Security/Floating_rate_notes.R")
source("./lib/classDefinitions/Security/Fondi_misti.R")
source("./lib/classDefinitions/Security/Fondi_mercato_monetario.R")
source("./lib/classDefinitions/Security/Diritti_aumento_capitale_azionario.R")
source("./lib/classDefinitions/SelectionCriterium/SelectionCriterium.R")
source("./lib/classDefinitions/SelectionCriterium/SecuritySelectionCriterium.R")
source("./lib/classDefinitions/SelectionCriterium/AmountSelectionCriterium.R")
source("./lib/classDefinitions/SelectionCriterium/CurrencySelectionCriterium.R")
source("./lib/classDefinitions/SelectionCriterium/MaturityHorizonSelectionCriterium.R")
source("./lib/classDefinitions/SelectionCriterium/SelectionCriteria.R")
source("./lib/classDefinitions/SelectionCriterium/SelectionCriteriaList.R")
source("./lib/classDefinitions/TestSuite/TestSuite.R")
source("./lib/classDefinitions/TestSuiteCollection/TestSuiteCollection.R")


source("./lib/classDefinitions/Id/Id.R")
source("./lib/classDefinitions/Position/Position.R")
source("./lib/classDefinitions/Position/PositionEquity/PositionEquity.R")
source("./lib/classDefinitions/Position/PositionBond/PositionBond.R")
source("./lib/classDefinitions/Position/PositionBond/AccruedInterest.R")
source("./lib/classDefinitions/Positions/Positions.R")
source("./lib/classDefinitions/Portfolio/Portfolio.R")


source("./lib/setMethod/securityFactory/securityFactory.R") 
source("./lib/setMethod/positionFactory/positionFactory.R")
source("./lib/setMethod/positionFactory/createPosition.R")
source("./lib/setMethod/positionsFactory/positionsFactory.R")
source("./lib/setMethod/positionsFactory/completeBondPosition.R")
source("./lib/setMethod/positionsFactory/matchToPositionBond.R")
source("./lib/setMethod/positionsFactory/adjustForAccruedInterest.R")
source("./lib/setMethod/testSuiteFactory/testSuiteFactory.R")
source("./lib/setMethod/constraintFactory/constraintFactory.R")
source("./lib/setMethod/selectionCriteriumFactory/selectionCriteriumFactory.R")
source("./lib/setMethod/check/check.R")
source("./lib/setMethod/selector/selector.R")
source("./lib/setMethod/selector/filterByCriteria.R")

source("./lib/setMethod/portfolioFactory/portfolioFactory.R")
source("./lib/setMethod/portfoliosFactory/portfoliosFactory.R")


source("./lib/setMethod/parser/parser.R")

# source("./lib/riskmanTestSuite.R")