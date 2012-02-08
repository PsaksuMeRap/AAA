# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Position/PositionBond/AccruedInterest.R")
source("./lib/classDefinitions/Position/Position.R")

# crea la classe virtuale "Position"
setClass("PositionBond",representation(accruedInterest="AccruedInterest"),contains="Position")


