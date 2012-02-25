# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./lib/classDefinitions/Security/Security.R")

setClass("Fondi_misti",representation(currency="Currency",name="character",id="Id"),contains="Security") 


