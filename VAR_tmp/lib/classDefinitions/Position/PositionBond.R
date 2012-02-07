# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Position/AccruedInterest.R")

# crea la classe virtuale "Position"
setClass("PositionBond",representation(accruedInterest="AccruedInterest"),contains="Position")


