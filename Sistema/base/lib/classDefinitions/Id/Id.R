# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Id/IdAyrton.R")

# crea la classe Id come classe virtuale di cui ID_Ayrton è un membro
setClassUnion("Id", c("IdAyrton","numeric"))
