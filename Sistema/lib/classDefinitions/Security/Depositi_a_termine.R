# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Depositi_a_termine",representation(currency="Currency",name="character",id="Id"),contains="Security") 

