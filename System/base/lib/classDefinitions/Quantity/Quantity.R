# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./base/lib/classDefinitions/Quantity/NominalValue.R")

# crea la classe virtuale Quantity utilizzata come slot quantità della classe
# Position
setClassUnion("Quantity",c("NominalValue","numeric"))
