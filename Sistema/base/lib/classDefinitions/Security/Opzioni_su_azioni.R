# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Opzioni_su_azioni",representation(currency="Currency",name="character",id="Id"),contains="Security") 

