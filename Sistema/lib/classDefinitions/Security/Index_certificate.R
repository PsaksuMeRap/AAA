# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Index_certificate",representation(currency="Currency",name="character",id="Id"),contains="Security") 

