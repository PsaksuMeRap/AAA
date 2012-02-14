# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldAddTwoPositions <- function() {
	# The class Positions is a container class (a list)
	# where the content is not checked for consistency
	
	# test1: two non empty positions
	e1 <- new("Positions",list("a","B",1:10))
	e2 <- new("Positions",list("Claudio",12.3))
	
	e3 <- union(e1,e2)
	
	checkEquals(e3,new("Positions",list("a","B",1:10,"Claudio",12.3)))
	
	# test2: two empty positions
	e1 <- new("Positions",list())
	e2 <- new("Positions",list())
	
	e3 <- union(e1,e2)
	
	checkEquals(e3,new("Positions",list()))
}
