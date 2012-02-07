# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Security/Security.R")

setClass("Bond",representation(name="character",id="Id",maturity="character"),contains="Security")


