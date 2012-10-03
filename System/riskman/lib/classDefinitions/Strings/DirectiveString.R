# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("DirectiveString",contains="character")

setClass("DirectiveStringElement",contains="DirectiveString")


setMethod("split",signature(x = "DirectiveStringElement"),
		function (x, f, drop = FALSE, ...) 
		{
			firstSplit <- removeStartEndSpaces(unlist(strsplit(x,":")))
			
			if (firstSplit[[1]]=="replace") {
				secondSplit <- removeStartEndSpaces(unlist(strsplit(firstSplit[[2]],",")))
				return(new("ReplaceDirective",secondSplit))
			}
			
			if (firstSplit[[1]]=="explode") {
				return(new("ExplodeDirective",firstSplit[[2]]))
			}
			
			if (firstSplit[[1]]=="groupBy") {
				return(new("GroupByDirective",firstSplit[[2]]))
			}
			
			stop(paste("Error: unknown DirectiveStringElement\n",x,sep=""))
		}
)


setMethod("split",
		signature(x = "DirectiveString"),
		function (x, f, drop = FALSE, ...) 
		{
			firstSplit <- unlist(strsplit(x,"&"))
			
			if (length(firstSplit)==0) stop(paste("Error: empty directiveString",x,sep="\n"))
			firstSplit <- removeStartEndSpaces(firstSplit)
			
			# create a wrapper of new() which reverse the arguments order
			New <- function(element,type) return(new(type,element))
			
			return(lapply(lapply(firstSplit,New,"DirectiveStringElement"),split))
		}
)