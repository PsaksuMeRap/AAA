# TODO: Add comment
# 
# Author: claudio
###############################################################################

## IDAAA_numeric is depecrated
setClass("IdAAA_numeric",contains="numeric")
setClass("IdAAA_character",contains="character")

setClassUnion("IdAAA", c("IdAAA_numeric","IdAAA_character"))

setClass("IdAyrton",representation(idAAA="IdAAA",idStrumento="numeric"))

# estendi la classe virtuale Id con ID_Ayrton
setIs("IdAyrton","Id")
