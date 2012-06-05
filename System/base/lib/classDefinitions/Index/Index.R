# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("Index",contains="VIRTUAL")

setClass("IndexEquity",representation(name="character",id="Id"),contains="Index")
