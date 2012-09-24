# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailWithNA <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	equity1 <- repository$equity1
	equity1@ID_AAA <- NA_real_
	
	checkException(idFactory(equity1))
}


test.createIdForInstrumentsWithIsin <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	
	#1
	equity1 <- repository$equity1
	#2
	bond1 <- repository$bond1
	#3
	fondiObbligazionari <- repository$fondiObbligazionari
	#4
	fondiMercatoMonetario <- repository$Fondi_mercato_monetario1
	#5
	floatingRateNotes <- repository$Floating_rate_notes1
	#11
	obbligazioniConvertibili <- repository$Obbligazioni_convertibili
	#14
	fondiAzionari <- repository$Fondi_azionari1
	#15
	indexCertificate <- repository$Index_certificate1
	# metalliPreziosi <- repository$Metalli_preziosi1
	#25
	fondiImmobiliari <- repository$Fondi_immobiliari1
	#26
	fondiMisti <- repository$Fondi_misti
	#30
	dirittiAumentoCapitaleAzionario <- repository$Diritti_aumento_capitale_azionario1
	#49
	strutturatiFI <- repository$strutturati_FI
	#50
	# futuresEQ <- repository$Futures_EQ1
	#44
	FondiHedge <- repository$Fondi_Hedge1
	#45
	etfEquity <- repository$ETF_equity1
	#51
	etfCommodity <- repository$ETF_commodities
	#52
	creditLinkedNote <- repository$Credit_linked_note

	# check creditLinkedNote
	result <- idFactory(creditLinkedNote)
	checkEquals(as.character(result@idAAA),"14723815")
	checkEquals(as.numeric(result@idStrumento),52)
	
	# check etfCommodity
	result <- idFactory(etfCommodity)
	checkEquals(as.character(result@idAAA),"GB00B00FHZ82")
	checkEquals(as.numeric(result@idStrumento),51)
	
	# check etfEquity
	result <- idFactory(etfEquity)
	checkEquals(as.character(result@idAAA),"FR0010346205")
	checkEquals(as.numeric(result@idStrumento),45)
	
	# check fondiHedge
	result <- idFactory(fondiHedge)
	checkEquals(as.character(result@idAAA),"10063727CH")
	checkEquals(as.numeric(result@idStrumento),44)
	
#	# check futuresEQ
#	result <- idFactory(futuresEQ)
#	checkEquals(as.character(result@idAAA),"11439214")
#	checkEquals(as.numeric(result@idStrumento),49)	
	
	# check strutturati_FI
	result <- idFactory(strutturatiFI)
	checkEquals(as.character(result@idAAA),"11439214")
	checkEquals(as.numeric(result@idStrumento),49)	
	
	# check dirittiAumentoCapitaleAzionario
	result <- idFactory(dirittiAumentoCapitaleAzionario)
	checkEquals(as.character(result@idAAA),"14742829CH")
	checkEquals(as.numeric(result@idStrumento),30)	
	
	# check fondiMisti
	result <- idFactory(fondiMisti)
	checkEquals(as.character(result@idAAA),"279211CH")
	checkEquals(as.numeric(result@idStrumento),26)	
	
	# check fondiImmobiliari
	result <- idFactory(fondiImmobiliari)
	checkEquals(as.character(result@idAAA),"1968401EU")
	checkEquals(as.numeric(result@idStrumento),25)
	
	# check indexCertificate
	result <- idFactory(indexCertificate)
	checkEquals(as.character(result@idAAA),"US46429B3096")
	checkEquals(as.numeric(result@idStrumento),15)
	
	# check fondiAzionari
	result <- idFactory(fondiAzionari)
	checkEquals(as.character(result@idAAA),"1021903EU")
	checkEquals(as.numeric(result@idStrumento),14)
	
	# check obbligazioniConvertibili
	result <- idFactory(obbligazioniConvertibili)
	checkEquals(as.character(result@idAAA),"CH0190462702")
	checkEquals(as.numeric(result@idStrumento),11)
	
	# check floatingRateNotes
	result <- idFactory(floatingRateNotes)
	checkEquals(as.character(result@idAAA),"2122570B")
	checkEquals(as.numeric(result@idStrumento),5)
	
	# check fondiMercatoMonetario
	result <- idFactory(fondiMercatoMonetario)
	checkEquals(as.character(result@idAAA),"1968401EU")
	checkEquals(as.numeric(result@idStrumento),4)	
	
	# check fondiObbligazionari
	result <- idFactory(fondiObbligazionari)
	checkEquals(as.character(result@idAAA),"2490099")
	checkEquals(as.numeric(result@idStrumento),3)
	
	# check bond1
	result <- idFactory(bond1)
	checkEquals(as.character(result@idAAA),"10234542")
	checkEquals(as.numeric(result@idStrumento),2)
	
	# check equity1
	result <- idFactory(equity1)
	checkEquals(as.character(result@idAAA),"1203204CH")
	checkEquals(as.numeric(result@idStrumento),1)

}

test.createIdForInstrumentsWithoutIsin <- function() {

	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	
	#6
	anticipoFisso <- repository$Anticipi_fissi1
	#7
	DepositoATermine <- repository$Deposito_a_termine1

	# check DepositoATermine
	result <- idFactory(DepositoATermine)
	checkEquals(as.character(result@idAAA),"Deposito singolo 01-04-09/02-04-12 deposito a termine al 2.05%")
	checkEquals(as.numeric(result@idStrumento),7)
	
	# check anticipoFisso
	result <- idFactory(anticipoFisso)
	checkEquals(as.character(result@idAAA),"Anticipo fisso 01-04-09/02-04-12 Ipoteca tasso fisso 115.000 CHF 2.05%")
	checkEquals(as.numeric(result@idStrumento),6)
	
}

test.createIdForOptionInstruments <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	
	opzioniSuAzioni <- repository$Opzioni_su_azioni1
	
	# check opzioniSuAzioni
	result <- idFactory(opzioniSuAzioni)
	
}