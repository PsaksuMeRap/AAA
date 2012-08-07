# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailToApplyReplaceDirective <- function() {
	replaceDirective <- new("ReplaceDirective","aiuto")
	positions <- new("Positions")
	
	checkException(Apply(replaceDirective,positions))

}


test.shouldFailToApplyReplaceDirective <- function() {
	replaceDirective <- new("ReplaceDirective","PositionOpzioni_su_azioni")
	positions <- new("Positions")
	
	checkEquals(Apply(replaceDirective,positions),TRUE)
	
}