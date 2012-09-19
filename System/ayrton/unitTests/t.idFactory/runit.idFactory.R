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
	equity1 <- repository$equity1
	bond1 <- repository$bond1
	fondiObbligazionari <- repository$fondiObbligazionari
	FondiMercatoMonetario <- repository$Fondi_mercato_monetario1
	FloatingRateNotes <- repository$Floating_rate_notes1
	
	obbligazioniConvertibili <- repository$Obbligazioni_convertibili
	fondiAzionari <- repository$Fondi_azionari1
	indexCertificate <- repository$Index_certificate1
	# metalliPreziosi <- repository$Metalli_preziosi1
	fondiImmobiliari <- repository$Fondi_immobiliari1
	fondiMisti <- repository$Fondi_misti
	dirittiAumentoCapitaleAzionario <- repository$Diritti_aumento_capitale_azionario1
	
	
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
	
	# check FloatingRateNotes
	result <- idFactory(FloatingRateNotes)
	checkEquals(as.character(result@idAAA),"2122570B")
	checkEquals(as.numeric(result@idStrumento),5)
	
	# check FondiMercatoMonetario
	result <- idFactory(FondiMercatoMonetario)
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
