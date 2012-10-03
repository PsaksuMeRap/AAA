# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailWithInvalidGroupByDirective <- function() {
	directive <- new("GroupByDirective","pippo123")
	positions <- new("Positions")
	
	checkException(groupByDirective(directive,positions))
}

test.shouldGroupByDirective <- function() {
	directive <- new("GroupByDirective","securityId")
	positions <- new("Positions")
	
	checkEquals(groupByDirective(directive,positions),positions)
}