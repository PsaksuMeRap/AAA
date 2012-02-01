# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.security <- function() {
	setClass("Security",
			representation(name="character"),
			prototype(name=NA_character_),
			contains="VIRTUAL"
	)
	
	setClass("Equity",representation(test1="numeric"),contains="Security") 
	
	a <- new("Equity",test1=10,name="test")
}
