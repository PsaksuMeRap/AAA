# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Money/Money.R")

# Crea la classe AccruedInterest utilizzata come slot nella posizione fixed income
setClass("AccruedInterest",representation(moneyAmount="Money"))


