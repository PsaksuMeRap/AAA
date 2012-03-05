# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClassUnion("ParsedFactorStringValue", c("vector","ConstraintString"))
setClass("ParsedFactorString",representation(criterium="character",negation="logical",values="ParsedFactorStringValue"))