# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Strutturati_FI",representation(currency="Currency",name="character",id="Id"),contains="Security") 

