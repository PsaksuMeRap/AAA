# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("SelectionCriterium",
		representation(values="vector",
				negation="logical",
				constraint="Constraint"
		)
)

# values: the values to verify, i.e. equity, USD, a currency (for factor amount)...
# negation: should the complement of the specified values be considered?
# constraint: an object of class Constraint

# factor: the class of the criterium, i.e. instrument, currency, amount, ...
