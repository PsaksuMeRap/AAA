# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Metalli_preziosi",representation(currency="Currency",name="character",id="Id"),contains="Security") 

