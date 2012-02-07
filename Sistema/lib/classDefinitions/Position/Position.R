# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/classDefinitions/Money/Money.R")
source("./lib/classDefinitions/Quantity/Quantity.R")
source("./lib/classDefinitions/Security/Security.R")

# crea la classe virtuale "Position"
setClass("Position",representation(id="Id",security="Security",quantity="Quantity",value="Money"))

