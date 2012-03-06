# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClass("Constraint",
		representation(operator="character"),prototype(operator=NA_character_)
)
# - the operator, i.e. > or ==


setClass("RelativeConstraint",
		representation(operator="character",value="numeric"),
		contains="Constraint",
		prototype(operator=NA_character_,value=NA_real_))
# - i.e. >5% gives operator=">", value=5

setClass("AbsoluteConstraint",
		representation(operator="character",value="Money"),
		contains="Constraint",
		prototype(operator=NA_character_,value=new("Money")))
# - i.e. >5 CHF gives operator=">", value=toMoney(5,"CHF")