# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Anticipi_fissi",representation(currency="Currency",name="character",id="Id"),contains="Security") 

