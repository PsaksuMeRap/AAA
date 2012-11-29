# TODO: Add comment
# 
# Author: claudio
###############################################################################

setGeneric("applyGroupDefinition",def=function(x,...) standardGeneric("applyGroupDefinition"))

setMethod("applyGroupDefinition",signature(x="TestSuiteParsed"),
		function(x) {
			# x: a parsedTestSuite
	
			# identify if groups have been defined
			isGroupDefinition <- names(x@configLines)=="groupDefinition"
			if (!any(isGroupDefinition)) return(x)
			
			# identify if checkStrings have been defined
			if (length(x@checkStrings)==0) return(x)
			
			for (groupDefinition in x@configLines[isGroupDefinition]) {
				info <- strsplit(groupDefinition,"=")[[1]]
				info <- remSpaces(info)
				x@checkStrings <- lapply(x@checkStrings,applyGroupDefinition,info)
			}
			return(x)
		}
)


setMethod("applyGroupDefinition",signature(x="CheckString"),
		function(x,groupDefinition) {
			# x: a checkString
			# groupDefinition: a vector with two components, first the name of the group
			# and then the corresponding values, separeted by comma. Example:
			# groupDefinition = c("allowedCurrencies","CHF,USD,GBP,EUR")

			result <- sub(groupDefinition[[1]],groupDefinition[[2]],x)
			result <- new("CheckString",result)
			return(result)
			
		}
)