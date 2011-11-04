# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_randomVariable <- function() {
	
	randomVariable <- create_randomVariable(name="")
	
	checkEquals(randomVariable$name,"")
	checkEquals(randomVariable$timeIndex,0)
	checkEquals(randomVariable$power,1)	
	checkEquals(class(randomVariable),c("RV","symbol"))

}

