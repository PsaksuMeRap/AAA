# TODO: Add comment
# 
# Author: claudio
###############################################################################


explode <- function(position) {
	# allo stato attuale questa procedura esplode solo Fondi_misti
	if (is.element("Fondi_misti",class(position))) {
		# crea posizione equityFund
		equityFundPosition <- create_position()
		equityFundPosition$create(
				name=paste("Fondo misto:",position$name),
				currency=position$money$currency,
				amount=position$money$amount*position$quotaEquities/100,
				origin=list(ID_AAA=NA,ID_strumento=14,Strumento="Fondo azionario")
		)
		class(equityFundPosition) <- c("Fondi_azionari","position")
		extendPosition(equityFundPosition)
		
		# crea posizione bondFund
		BondFundPosition <- create_position()
		BondFundPosition$create(
				name=paste("Fondo misto:",position$name),
				currency=position$money$currency,
				amount=position$money$amount*position$quotaBonds/100,
				origin=list(ID_AAA=NA,ID_strumento=3,Strumento="Fondo obbligazionario")
		)
		class(BondFundPosition) <- c("Fondi_obbligazionari","position")
		extendPosition(BondFundPosition)
		
		positions <- create_positions()
		positions$add(equityFundPosition)
		positions$add(BondFundPosition)
		return(positions)
	} else {
		return(position)
	}
}

