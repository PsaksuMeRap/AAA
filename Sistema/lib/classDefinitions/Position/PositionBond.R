# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Position/AccruedInterest.R")
source("./lib/classDefinitions/Position/Position.R")

# crea la classe virtuale "Position"
setClass("PositionBond",representation(accruedInterest="AccruedInterest"),contains="Position")


