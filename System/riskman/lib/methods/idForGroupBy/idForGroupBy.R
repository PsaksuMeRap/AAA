# TODO: Add comment
# 
# Author: claudio
###############################################################################

setGeneric("idForGroupBy",def=function(x) standardGeneric("idForGroupBy"))

setMethod("idForGroupBy",signature(x="IdCharacter"),
		function(x) {
			# x: an id of class IdCharacter
			
			return(as.character(x))
		}
)

setMethod("idForGroupBy",signature(x="numeric"),
		function(x) {
			# x: an id of class numeric
			
			return(as.character(x))
		}
)

setMethod("idForGroupBy",signature(x="IdAyrton"),
		function(x) {
			# x: an id of class IdAyrton
			
			return(paste(as.character(x@idAAA),as.character(x@idStrumento),sep="__"))
		}
)
