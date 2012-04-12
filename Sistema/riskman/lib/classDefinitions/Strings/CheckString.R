# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("CheckString",contains="character")

setMethod("split",
		signature(x = "CheckString"),
		function (x, f, drop = FALSE, ...) 
		{
			#checkString: a string like "instrument:equity & currency:CHF + instrument:bond ; <=3% :: explode:Fondo_misto"
			# i.e. the union of a selectionString, a constraintString and a directive string, separated by a 
			# a semicolon and ::, respectively
			# the selectionString is mandatory, the constraintString and directive string are optional
			
			firstSplit <- unlist(strsplit(x,";"))
			
			if (length(firstSplit)==0) stop(paste("Error: empty checkString",x,sep="\n"))
			firstSplit <- removeStartEndSpaces(firstSplit)
			
			if (length(firstSplit)==1)  {
				constraintString <- new("ConstraintString",NA_character_)
				secondSplit <- unlist(strsplit(firstSplit[[1]],"::"))
				secondSplit <- removeStartEndSpaces(secondSplit)
				if (length(secondSplit)==1)  {
					directiveString <- new("DirectiveString",NA_character_)
					selectionString <- new("SelectionString",secondSplit[[1]])
				} else {
					directiveString <- new("DirectiveString",secondSplit[[2]])
					selectionString <- new("SelectionString",secondSplit[[1]])				
				}	
			} else {
				selectionString <- new("SelectionString",firstSplit[[1]])
				
				secondSplit <- unlist(strsplit(firstSplit[[2]],"::"))
				secondSplit <- removeStartEndSpaces(secondSplit)
				
				if (length(secondSplit)==1)  {
					directiveString  <- new("DirectiveString",NA_character_)
					constraintString <- new("ConstraintString",secondSplit[[1]])
				} else {
					directiveString <- new("DirectiveString",secondSplit[[2]])
					constraintString <- new("ConstraintString",secondSplit[[1]])				
				}				
			}
			
			x <- list(selectionString=selectionString,constraintString=constraintString,
					directiveString=directiveString)		
			return(x)
		}
)