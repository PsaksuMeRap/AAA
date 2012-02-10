# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Fondi_obbligazionari",representation(name="character",id="Id"),contains="Security") 

