# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Bond",representation(currency="Currency",name="character",id="Id",maturity="character"),contains="Security")


