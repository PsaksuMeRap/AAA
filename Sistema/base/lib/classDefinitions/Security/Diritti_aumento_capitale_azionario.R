# TODO: Add comment
# 
# Author: claudio
###############################################################################


source("./lib/classDefinitions/Security/Security.R")

setClass("Diritti_aumento_capitale_azionario",representation(currency="Currency",name="character",id="Id"),
		contains="Security") 


