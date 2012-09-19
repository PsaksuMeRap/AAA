# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("IdAAA_numeric",contains="numeric")
setClass("IdAAA_character",contains="character")

setClassUnion("IdAAA", c("IdAAA_numeric","IdAAA_character"))

setClass("IdAyrton",representation(idAAA="IdAAA",idStrumento="numeric"))

# estendi la classe virtuale Id con ID_Ayrton
setIs("IdAyrton","Id")

setMethod("identical",
		signature(x = "IdAAA_numeric", y = "AyrtonPosition"),
		function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE) 
		{
			return(identical(as.numeric(x),y@ID_AAA))
		}
)

setMethod("identical",
		signature(x = "IdAAA_string", y = "AyrtonPosition"),
		function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE) 
		{
			return(identical(as.character(x),y@Nome))
		}
)