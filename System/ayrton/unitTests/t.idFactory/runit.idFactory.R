# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailWithNA <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	equity1 <- repository$equity1
	equity1@ID_strumento <- 205526 
	
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
	#21
	metalliPreziosi <- repository$Metalli_preziosi1
	#22
	fxForward <- repository$FX_Forward1
	#25
	fondiImmobiliari <- repository$Fondi_immobiliari1
	#26
	fondiMisti <- repository$Fondi_misti
	#30
	dirittiAumentoCapitaleAzionario <- repository$Diritti_aumento_capitale_azionario1
	#40 
	contoCorrente <- repository$Conto_corrente1
	#49
	strutturatiFI <- repository$strutturati_FI
	#50
	futuresEQ <- repository$Futures_EQ1
	#44
	fondiHedge <- repository$Fondi_Hedge1
	#45
	etfEquity <- repository$ETF_equity1
	#51
	etfCommodityGold <- repository$ETF_commodities_gold
	#52
	creditLinkedNote <- repository$Credit_linked_note
	#55
	etfCommodityPlatinum <- repository$ETF_commodities_platinum
	
	# check creditLinkedNote
	result <- idFactory(creditLinkedNote)
	checkEquals(as.character(result@idAAA),"14723815")
	checkEquals(as.numeric(result@idStrumento),52)
	
	# check etfCommodityPlatinum
	result <- idFactory(etfCommodityPlatinum)
	checkEquals(as.character(result@idAAA),"CH0116014934")
	checkEquals(as.numeric(result@idStrumento),55)
	
	# check etfCommodityGold
	result <- idFactory(etfCommodityGold)
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
	
	# check futuresEQ
	result <- idFactory(futuresEQ)
	checkEquals(as.character(result@idAAA),"SMI Futures2012-03-16")
	checkEquals(as.numeric(result@idStrumento),50)	
	
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
	
	# check fxForward
	result <- idFactory(fxForward)
	checkEquals(as.character(result@idAAA),"CHF26-03-2012")
	checkEquals(as.numeric(result@idStrumento),22)
	
	# check Conto_corrente
	result <- idFactory(contoCorrente)
	checkEquals(as.character(result@idAAA),"CHF-chf")
	checkEquals(as.numeric(result@idStrumento),40)
	
	# check MetalliPreziosi
	result <- idFactory(metalliPreziosi)
	checkEquals(as.character(result@idAAA),"XAU")
	checkEquals(as.numeric(result@idStrumento),21)
	
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
	checkEquals(result@idAAA,new("IdAAA_character","CH0011027469290C17-02-2012"))
	checkEquals(result@idStrumento,18)
	
	opzioniSuDivise <- repository$Opzioni_su_divise1
	
	# check opzioniSuDivise
	result <- idFactory(opzioniSuDivise)
	checkEquals(result@idAAA,new("IdAAA_character","EURUSD1.295P17-08-2012"))
	checkEquals(result@idStrumento,19)
	
	opzioniSuObbligazioni <- repository$opzioni_su_obbligazioni
	
	# check opzioniSuObbligazioni
	result <- idFactory(opzioniSuObbligazioni)
	checkEquals(result@idAAA,new("IdAAA_character","EU0011027469103.5P17-08-2012"))
	checkEquals(result@idStrumento,20)
}

