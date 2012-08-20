# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailToApplyReplaceDirective <- function() {
	replaceDirective <- new("ReplaceDirective","aiuto")
	positions <- new("Positions")
	
	checkException(Apply(replaceDirective,positions))

}


test.shouldApplyReplaceDirectiveOpzioni_su_azioni_1 <- function() {
	replaceDirective <- new("ReplaceDirective","Opzioni_su_azioni")
	positions <- new("Positions")
	
	checkEquals(Apply(replaceDirective,positions),new("Positions"))
	
}

test.shouldApplyReplaceDirectiveOpzioni_su_azioni_1 <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$Opzioni_su_azioni2,repository$Opzioni_su_divise2,
			repository$Opzioni_su_azioni1,repository$Opzioni_su_divise4)
	
	directive <- new("ReplaceDirective",c("Opzioni_su_azioni"))
	
	result <- Apply(directive,positions)
	
	checkEquals(length(result),6)
	checkEquals(is(result[[1]]),)
}