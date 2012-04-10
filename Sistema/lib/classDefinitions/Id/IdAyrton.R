# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("IdAAA_numeric",contains="numeric")
setClass("IdAAA_string",contains="character")

setClassUnion("IdAAA", c("IdAAA_numeric","IdAAA_string"))

setClass("IdAyrton",representation(idAAA="IdAAA",idStrumento="numeric"))
