# TODO: Add comment
# 
# Author: claudio
###############################################################################



explodeFundFixedIncome <- function(position,percentages) {	
	# This procedure explodes the security OnCapital Global Equity Fund Cap B
		
	if (position@security@id@idAAA=="LU0810451608") {
		listOfPositions <- list()
		
		nbPos <- 0
		# create a security "Fondi_azionari"
		if (percentages$Eq!=0) {
			nbPos <- 1
			currency <- position@security@currency
			name <- paste("Eq. part of:",position@security@name)
			
			equityFundSecurity <- new("Fondi_azionari",currency=currency,name=name,id=position@security@id) 
			
			# create the corresponding equityFund position 
			quantity <- percentages$Eq*position@quantity
			value <- toMoney(percentages$Eq*position@value@amount,position@value@currency)
			id <- position@id
			
			listOfPositions[[nbPos]] <- new("Position",id=id,security=equityFundSecurity,
					quantity=quantity,value=value)				
		}
		
		# create a security "Fondi_obbligazionari"
		if (percentages$Fi!=0) {
			nbPos <- nbPos + 1
			currency <- position@security@currency
			name <- paste("FI part of:",position@security@name)
			idAAANumeric <- new("IdAAA_numeric",-1)
			idAyrton <- new("IdAyrton",idAAA=idAAANumeric,idStrumento=3) 
			bondFundSecurity <- new("Fondi_obbligazionari",currency=currency,name=name,id=idAyrton) 
			
			# create the corresponding equityFund position 
			quantity <- percentages$Fi*position@quantity
			value <- toMoney(percentages$Fi*position@value@amount,position@value@currency)
			
			listOfPositions[[nbPos]] <- new("Position",id=idAyrton,security=bondFundSecurity,
					quantity=quantity,value=value)
		}
		
		
		# create a security "Conto_corrente_fittizio"
		liquidity <- 1 - percentages$Eq - percentages$Fi
		if (liquidity!=0) {
			nbPos <- nbPos + 1
			currency <- position@security@currency
			name <- paste("Liq. part of:",position@security@name)
			idAAANumeric <- new("IdAAA_numeric",-1)
			idAyrton <- new("IdAyrton",idAAA=idAAANumeric,idStrumento=54) 
			
			contoCorrenteFittizioSecurity <- new("Conto_corrente",currency=currency,name=name,id=idAyrton)
			
			# create the corresponding position
			quantity <- liquidity*position@quantity
			value <- toMoney(liquidity*position@value@amount,position@value@currency)
			id <- idAyrton
			
			listOfPositions[[nbPos]] <- new("Position",id=id,security=contoCorrenteFittizioSecurity,
					quantity=quantity,value=value)
		}
		
		return(new("Positions",listOfPositions))
	} else {
		return(position)
	}
}
		
