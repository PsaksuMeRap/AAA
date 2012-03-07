# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("SecuritySelectionCriterium",representation(values="character",negation="logical"),
		contains="SelectionCriterium"
)
