# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCompareTwoRatingsSP <- function() {
	
	e1 <- longTermRatingFactory("AA")
	e2 <- longTermRatingFactory("B")
	
	# check >
	checkEquals(e1>e2,TRUE)
	
	# check >=	
	checkEquals(e1>=e2,TRUE)
	
	# check <
	checkEquals(e1<e2,FALSE)
	
	# check <=
	checkEquals(e1<=e2,FALSE)
	
	# check !=
	checkEquals(e1!=e2,TRUE)
	
	# check ==
	e1 <- e2
	checkEquals(e1==e2,TRUE)
	
}


test.shouldCompareTwoRatingsMoody <- function() {
	
	e1 <- longTermRatingFactory("Aa1")
	e2 <- longTermRatingFactory("B3")
	
	# check >
	checkEquals(e1>e2,TRUE)
	
	# check >=	
	checkEquals(e1>=e2,TRUE)
	
	# check <
	checkEquals(e1<e2,FALSE)
	
	# check <=
	checkEquals(e1<=e2,FALSE)
	
	# check !=
	checkEquals(e1!=e2,TRUE)
	
	# check ==
	e1 <- e2
	checkEquals(e1==e2,TRUE)
	
}

test.shouldCompareRatingSPvsMoody <- function() {
	
	e1 <- longTermRatingFactory("AAA")
	e2 <- longTermRatingFactory("B3")
	
	# check >
	checkEquals(e1>e2,TRUE)
	
	# check >=	
	checkEquals(e1>=e2,TRUE)
	
	# check <
	checkEquals(e1<e2,FALSE)
	
	# check <=
	checkEquals(e1<=e2,FALSE)
	
	# check !=
	checkEquals(e1!=e2,TRUE)
	
	# check ==
	e1 <- e2
	checkEquals(e1==e2,TRUE)
	
	# check equality with Moody "Ca"
	e1 <- longTermRatingFactory("C")
	e2 <- longTermRatingFactory("Ca")
	checkEquals(e1==e2,TRUE)
}


test.shouldCompareRatingSPvsMoody <- function() {
	
	e1 <- longTermRatingFactory("Aaa")
	e2 <- longTermRatingFactory("A-")
	
	
	# check >
	checkEquals(e1>e2,TRUE)
	
	# check >=	
	checkEquals(e1>=e2,TRUE)
	
	# check <
	checkEquals(e1<e2,FALSE)
	
	# check <=
	checkEquals(e1<=e2,FALSE)
	
	# check !=
	checkEquals(e1!=e2,TRUE)
	
	# check ==
	e1 <- e2
	checkEquals(e1==e2,TRUE)
	
	# check equality with Moody "Ca"
	e2 <- longTermRatingFactory("C")
	e1 <- longTermRatingFactory("Ca")
	checkEquals(e1==e2,TRUE)
}
