# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Equity",representation(name="character",id="Id"),contains="Security") 

