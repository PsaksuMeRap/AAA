# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./riskman/lib/classDefinitions/Constraint/Constraint.R") 
source("./riskman/lib/classDefinitions/Strings/CheckString.R")
source("./riskman/lib/classDefinitions/SelectionCriterium/SelectionCriteriaList.R")
source("./riskman/lib/classDefinitions/Directive/Directive.R")
source("./riskman/lib/classDefinitions/Strings/DirectiveString.R")
source("./riskman/lib/classDefinitions/Strings/CheckStringParsed.R")
source("./riskman/lib/classDefinitions/Strings/ConstraintString.R")
source("./riskman/lib/classDefinitions/Strings/FactorString.R")
source("./riskman/lib/classDefinitions/Strings/FactorStrings.R")
source("./riskman/lib/classDefinitions/Strings/SelectionString.R")
source("./riskman/lib/classDefinitions/Strings/FactorStringParsed.R")


source("./riskman/lib/classDefinitions/SelectionCriterium/SelectionCriterium.R")
source("./riskman/lib/classDefinitions/SelectionCriterium/SecuritySelectionCriterium.R")
source("./riskman/lib/classDefinitions/SelectionCriterium/AmountSelectionCriterium.R")
source("./riskman/lib/classDefinitions/SelectionCriterium/CurrencySelectionCriterium.R")
source("./riskman/lib/classDefinitions/SelectionCriterium/MaturityHorizonSelectionCriterium.R")
source("./riskman/lib/classDefinitions/SelectionCriterium/SelectionCriteria.R")


source("./riskman/lib/classDefinitions/TestSuite/TestSuite.R")
source("./riskman/lib/classDefinitions/TestSuiteCollection/TestSuiteCollection.R")


source("./riskman/lib/methods/constraintFactory/constraintFactory.R")
source("./riskman/lib/methods/selectionCriteriumFactory/selectionCriteriumFactory.R")
source("./riskman/lib/methods/check/check.R")
source("./riskman/lib/methods/selector/selector.R")
source("./riskman/lib/methods/selector/filterByCriteria.R")
source("./riskman/lib/methods/replaceDirective/replaceDirective.R")

source("./riskman/lib/methods/parser/parser.R")
source("./riskman/lib/methods/Apply/Apply.R")


source("./riskman/lib/methods/testSuiteFactory/testSuiteFactory.R")
source("./riskman/lib/methods/applyTestSuite/applyTestSuite.R")

source("./riskman/lib/methods/applyGroupDefinition/applyGroupDefinition.R")
source("./riskman/lib/methods/idForGroupBy/idForGroupBy.R")
source("./riskman/lib/methods/groupBy/groupBy.R")
source("./riskman/lib/methods/groupByDirective/groupByDirective.R")