# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("DirectiveString",contains="character")

setMethod("split",
		signature(x = "DirectiveString"),
		function (x, f, drop = FALSE, ...) 
		{
			# DirectiveString: a string like "explode:Fondi_misti & replace:PositionOpzioni_su_azioni,PositionOpzioni_su_divise"
			
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