# TODO: Add comment
# 
# Author: claudio
###############################################################################

setGeneric("createPosition",
		useAsDefault=function(security,origin) {
			id <- 10.2
			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency",origin@Moneta))
			position <- new("Position",id=id,security=security,
					quantity=quantity,value=value)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="NULL",origin="AyrtonPosition"),
		function(security,origin) {
			# if security is of class NULL, i.e. from an AccruedInterest
			# return NULL
			return(NULL)
		}
)

setMethod("createPosition",signature(security="Equity",origin="AyrtonPosition"),
		function(security,origin) {
			id <- 10.2
			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency",origin@Moneta))
			position <- new("PositionEquity",id=id,security=security,
					quantity=quantity,value=value)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Bond",origin="AyrtonPosition"),
		function(security,origin) {
			# the position will be completed with the accruedInterest after an
			# appropriate call to the createPositions method
			id <- 10.2
			quantity <- new("NominalValue",amount=new("Amount",origin@Saldo),currency=new("Currency",origin@Moneta))
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency",origin@Moneta))
			accruedInterest <- new("AccruedInterest",toMoney(NA_real_,origin@Moneta))
			position <- new("PositionBond",id=id,security=security,
					quantity=quantity,value=value,accruedInterest=accruedInterest)
			
			return(position)
		}
)
