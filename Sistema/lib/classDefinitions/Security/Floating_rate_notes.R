# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Floating_rate_notes",representation(currency="Currency",name="character",id="Id"),contains="Security") 

