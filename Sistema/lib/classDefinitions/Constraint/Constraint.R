# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClass("Constraint",
		representation(operator="character"),prototype(operator=NA_character_)
)
# - the operator, i.e. > or ==


setClass("RelativeConstraint",
		representation(value="numeric"),
		contains="Constraint",
		prototype(value=NA_real_))
# - i.e. >5% gives operator=">", value=5

setClass("AbsoluteConstraint",
		representation(value="Money"),
		contains="Constraint",
		prototype(value=new("Money")))
# - i.e. >5 CHF gives operator=">", value=toMoney(5,"CHF")