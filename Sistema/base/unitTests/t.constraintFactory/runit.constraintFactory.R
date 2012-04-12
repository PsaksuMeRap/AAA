# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateConstraint <- function() {
	
	constraintString1 <- "=0USD"
	constraintString1 <- new("ConstraintString",constraintString1)

	constraintString2 <- "<= 5.65%"
	constraintString2 <- new("ConstraintString",constraintString2)
		
	result1 <- constraintFactory(constraintString1)
	checkEquals(result1@operator,"=")
	checkEquals(result1@value,toMoney(0,"USD"))	
	
	result2 <- constraintFactory(constraintString2)
	checkEquals(result2@operator,"<=")
	checkEquals(result2@value,5.65)	
	
	
}

test.shouldFailOnInvalidConstraint <- function() {
	
	constraintString <- "!=100"
	constraintString <- new("ConstraintString",constraintString)
	
	checkException(constraintFactory(constraintString))
}