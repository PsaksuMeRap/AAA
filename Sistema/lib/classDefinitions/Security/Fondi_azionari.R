# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./lib/classDefinitions/Security/Security.R")

setClass("Fondi_azionari",representation(currency="Currency",name="character",id="Id"),contains="Security") 


