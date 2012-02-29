# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClassUnion("ConstraintValue", c("numeric","Money"))

setClass("Constraint",
		representation(operator="character",value="ConstraintValue",
				kind="character"),
		prototype(operator=NA_character_,value=NA_real_,kind=NA_character_))
# - the operator, i.e. > or ==
# - the value, i.e. 1450.75 or a variable of class money
# - the kind, i.e. absolute or relative
