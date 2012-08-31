# TODO: Add comment
# 
# Author: claudio
###############################################################################



createSecurity <- function(origin) UseMethod("createSecurity")

setGeneric("createSecurity",
		useAsDefault=function(origin) {

			# this is a common slot of all instruments
			idAyrton <- idFactory(origin)
			
			className <- class(origin)
			className <- substr(className,start=8,stop=nchar(className))
			security <- new(className,currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton)
			return(security)
		}
)