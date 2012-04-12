# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("FX_Forward",representation(currency="Currency",name="character",id="Id"),contains="Security") 

