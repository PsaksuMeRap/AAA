# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("SecuritySelectionCriterium",representation(values="vector",negation="logical"),
		contains="SelectionCriterium"
)
