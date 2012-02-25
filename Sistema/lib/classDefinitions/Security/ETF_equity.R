# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("ETF_equity",representation(currency="Currency",name="character",id="Id"),contains="Security") 

