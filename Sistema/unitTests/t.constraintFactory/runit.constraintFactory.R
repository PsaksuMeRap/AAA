# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateConstraint <- function() {
	
	constraintString1 <- "=0USD"
	constraintString1 <- new("ConstraintString",constraintString1)

	constraintString2 <- "<= 5.65%"
	constraintString2 <- new("ConstraintString",constraintString2)
	
	constraintString3 <- "!=d 0%"
	constraintString3 <- new("ConstraintString",constraintString3)
		
	result1 <- constraintFactory(constraintString1)
	checkEquals(result1@operator,"=")
	checkEquals(result1@value,toMoney(0,"USD"))	
	checkEquals(result1@kind,"absolute")
	
	result2 <- constraintFactory(constraintString2)
	checkEquals(result2@operator,"<=")
	checkEquals(result2@value,5.65)	
	checkEquals(result2@kind,"relative")
	
	checkException(constraintFactory(constraintString3))

	
}