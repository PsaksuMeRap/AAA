# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("replaceDirective",def=function(position,...) standardGeneric("replaceDirective"))

setMethod("replaceDirective",
		signature(position="PositionFutures_EQ"),
		function(position) {
			# parse the name and extract FUT_VAL_PT  <- verifica!
			
			name <- position@security@name
			splitResult <- strsplit(nome,"/")
			futureValueOfOnePoint <- splitResult[[1]][[length(splitResult)]]
			futureValueOfOnePoint <- as.numeric(futureValueOfOnePoint)
			positionValue <- position@ * position
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_azioni"),
		function(position) {
			
			
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_divise"),
		function(position) {
			
			
		}
)