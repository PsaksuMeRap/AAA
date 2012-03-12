# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("explode",def=function(position) standardGeneric("explode"))

setMethod("explode",signature(position="Position"),
		function(position) {	
			# This procedure explodes the security "Fondi_misti" only
			if (is(position@security,"Fondi_misti")) {
		
				# create a security "Fondi_azionari"
				currency <- position@security@currency
				name <- paste("Fondo misto:",position@security@name)
				idAyrton <- new("IdAyrton",idAAA=-1,idStrumento=14)
				
				equityFundSecurity <- new("Fondi_azionari",currency=currency,name=name,id=idAyrton) 
				
				# create the corresponding equityFund position 
				quantity <- position@equityPart*position@quantity/100
				value <- toMoney(position@value@amount*position@equityPart/100,position@value@currency)
				id <- position@id
				
				equityFundPosition <- new("Position",id=id,security=equityFundSecurity,
								quantity=quantity,value=value)				
				
				# create a security "Fondi_obbligazionari"
				currency <- position@security@currency
				name <- paste("Fondo misto:",position@security@name)
				idAyrton <- new("IdAyrton",idAAA=-1,idStrumento=3)
				
				bondFundSecurity <- new("Fondi_obbligazionari",currency=currency,name=name,id=idAyrton) 
				
				# create the corresponding bondFund position
				quantity <- position@bondPart*position@quantity/100
				value <- toMoney(position@value@amount*position@bondPart/100,position@value@currency)
				id <- position@id
				
				bondFundPosition <- new("Position",id=id,security=bondFundSecurity,
								quantity=quantity,value=value)				
				
				
				return(new("Positions",list(equityFundPosition,bondFundPosition)))
			} else {
				return(position)
			}
		}
)
