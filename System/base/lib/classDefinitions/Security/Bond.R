# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Bond",representation(currency="Currency",name="character",id="Id",maturity="character"),contains="Security")


