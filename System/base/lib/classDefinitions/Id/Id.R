# TODO: Add comment
# 
# Author: claudio
###############################################################################


# crea la classe Id come classe virtuale contenente "numeric" e "character"
setClass("IdCharacter",contains="character")
setClassUnion("Id",c("numeric","IdCharacter"))
