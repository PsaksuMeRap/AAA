# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("getMaturity",def=function(origin,...) standardGeneric("getMaturity"))

setMethod("getMaturity",signature(origin="Allocare_Bond"),
		function(origin) {
			# extract the maturity
			name <- origin@Nome
			paymentDate <- substr(name,nchar(name)-8+1,
					nchar(name))
			
			# verifica che il nome sia una data
			day <- substr(paymentDate,1,2)
			month <- substr(paymentDate,4,5)	
			year <- paste("20",substr(paymentDate,7,8),sep="")		
			paymentDate <- paste(year,month,day,sep="-")
			
			if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=paymentDate,perl=TRUE)) {
				message <- paste("Invalid date parsed",paymentDate,"for security:\n",name,"of type bond")
				stop(message)
			}
			return(paymentDate)
		}
)

setMethod("getMaturity",signature(origin="Ayrton_Bond_floater"),
		function(origin) {
			origin <- new("Ayrton_Bond",origin)
			return(getMaturity(origin))
		}
)

setMethod("getMaturity",signature(origin="Ayrton_Depositi_a_termine"),
		function(origin) {
			# extract the maturity
			name <- origin@Nome
			maturity <- substr(name,27,34)
			
			# verifica che il nome sia una data
			day <- substr(maturity,1,2)
			month <- substr(maturity,4,5)	
			year <- paste("20",substr(maturity,7,8),sep="")		
			maturity <- paste(year,month,day,sep="-")
			
			if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=maturity,perl=TRUE)) {
				message <- paste("Invalid date parsed",maturity,"for security:\n",name,"of type Depositi_a_termine")
				stop(message)
			}
			return(maturity)
		}
)


setMethod("getMaturity",signature(origin="Ayrton_Anticipi_fissi"),
		function(origin) {
			# extract the maturity
			name <- origin@Nome
			maturity <- substr(name,25,32)
			
			# verifica che il nome sia una data
			day <- substr(maturity,1,2)
			month <- substr(maturity,4,5)	
			year <- paste("20",substr(maturity,7,8),sep="")		
			maturity <- paste(year,month,day,sep="-")
			
			if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=maturity,perl=TRUE)) {
				message <- paste("Invalid date parsed",maturity,"for security:\n",name,"of type Anticipi_fissi")
				stop(message)
			}
			return(maturity)
		}
)


setMethod("getMaturity",signature(origin="Ayrton_Obbligazioni_convertibili"),
		function(origin) {

			origin <- new("Ayrton_Bond",origin)

			return(getMaturity(origin))
		}	
)