# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryPositions <- function() {

	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	
	testData <- new.env()

	# create a valid equity
	equity1 <- positionFactory(repository$equity1)
	testData$equity1 <- positionFactory(repository$equity1)

	# create a valid equity
	equity2 <- positionFactory(repository$equity2)
	testData$equity2 <- positionFactory(repository$equity2)
	
	# create a valid index certificate
	indexCertificate <- positionFactory(repository$indexCertificate)
	testData$indexCertificate <- positionFactory(repository$indexCertificate)
	
	# create a non existing equity
	noExists <- positionFactory(repository$noExists)
	testData$noExists <- positionFactory(repository$noExists)
	
	# create a valid bond
	bond1 <- positionFactory(repository$bond1)
	testData$bond1 <- positionFactory(repository$bond1)
	
	# create a valid bond
	bond2 <- positionFactory(repository$bond2)
	testData$bond2 <- positionFactory(repository$bond2)
	
	# create a valid bond
	bond3 <- positionFactory(repository$bond3)
	testData$bond3 <- positionFactory(repository$bond3)
	
	# create a valid bond (match to proRata1)
	bond4 <- positionFactory(repository$bond4)
	testData$bond4 <- positionFactory(repository$bond4)

	# create a valid AccruedInterest di Fondi_obbligazionari
	fondiObbligazionari <- positionFactory(repository$fondiObbligazionari)
	testData$fondiObbligazionari <- positionFactory(repository$fondiObbligazionari)
	
	# create a valid AccruedInterest di Fondi_obbligazionari
	proRataFondiObbligazionari <- positionFactory(repository$proRataFondiObbligazionari)
	testData$proRataFondiObbligazionari <- positionFactory(repository$proRataFondiObbligazionari)
	
	# create a valid AccruedInterest
	proRata1 <- positionFactory(repository$proRata1)
	testData$proRata1 <- positionFactory(repository$proRata1)
	
	# create an Unclassified security
	unclassified1 <- positionFactory(repository$unclassified1)
	testData$unclassified1 <- positionFactory(repository$unclassified1)
	
	# create a strutturati_FI 
	strutturati_FI <- positionFactory(repository$strutturati_FI)
	testData$strutturati_FI <- positionFactory(repository$strutturati_FI)
	
	# create a Fondi_misti 
	Fondi_misti <- positionFactory(repository$Fondi_misti)
	testData$Fondi_misti <- positionFactory(repository$Fondi_misti)
	
	# create a globalEquity 
	globalEquity <- positionFactory(repository$globalEquity)
	testData$globalEquity <- positionFactory(repository$globalEquity)
	
	# create a globalEconomy 
	globalEconomy <- positionFactory(repository$globalEconomy)
	testData$globalEconomy <- positionFactory(repository$globalEconomy)
	
	# create a fixedIncome 
	fixedIncome <- positionFactory(repository$fixedIncome)
	testData$fixedIncome <- positionFactory(repository$fixedIncome)
	

	Fondi_mercato_monetario1 <- positionFactory(repository$Fondi_mercato_monetario1)
	testData$Fondi_mercato_monetario1 <- positionFactory(repository$Fondi_mercato_monetario1)
	
	Fondi_mercato_monetario2 <- positionFactory(repository$Fondi_mercato_monetario2)
	testData$Fondi_mercato_monetario2 <- positionFactory(repository$Fondi_mercato_monetario2)
	
	Floating_rate_notes1 <- positionFactory(repository$Floating_rate_notes1)
	testData$Floating_rate_notes1 <- positionFactory(repository$Floating_rate_notes1)
	
	Floating_rate_notes2 <- positionFactory(repository$Floating_rate_notes2)
	testData$Floating_rate_notes2 <- positionFactory(repository$Floating_rate_notes2)
	
	Anticipi_fissiAccrual1 <- positionFactory(repository$Anticipi_fissiAccrual1)
	testData$Anticipi_fissiAccrual1 <- positionFactory(repository$Anticipi_fissiAccrual1)
	
	Anticipi_fissi1 <- positionFactory(repository$Anticipi_fissi1)
	testData$Anticipi_fissi1 <- positionFactory(repository$Anticipi_fissi1)
	
	Anticipi_fissiAccrual2 <- positionFactory(repository$Anticipi_fissiAccrual2)
	testData$Anticipi_fissiAccrual2 <- positionFactory(repository$Anticipi_fissiAccrual2)
	
	Anticipi_fissi2 <- positionFactory(repository$Anticipi_fissi2)
	testData$Anticipi_fissi2 <- positionFactory(repository$Anticipi_fissi2)
	
	Fondi_azionari1 <- positionFactory(repository$Fondi_azionari1)
	testData$Fondi_azionari1 <- positionFactory(repository$Fondi_azionari1)
	
	Fondi_azionari2 <- positionFactory(repository$Fondi_azionari2)
	testData$Fondi_azionari2 <- positionFactory(repository$Fondi_azionari2)
	
	Index_certificate1 <- positionFactory(repository$Index_certificate1)
	testData$Index_certificate1 <- positionFactory(repository$Index_certificate1)
	
	Index_certificate2 <- positionFactory(repository$Index_certificate2)
	testData$Index_certificate2 <- positionFactory(repository$Index_certificate2)
	
	Opzioni_su_azioni1 <- positionFactory(repository$Opzioni_su_azioni1)
	testData$Opzioni_su_azioni1 <- positionFactory(repository$Opzioni_su_azioni1)
	
	Opzioni_su_azioni2 <- positionFactory(repository$Opzioni_su_azioni2)
	testData$Opzioni_su_azioni2 <- positionFactory(repository$Opzioni_su_azioni2)
	
	
	Deposito_a_termine_acc1 <- positionFactory(repository$Deposito_a_termine_acc1)
	testData$Deposito_a_termine_acc1 <- positionFactory(repository$Deposito_a_termine_acc1)	
	
	Deposito_a_termine1 <- positionFactory(repository$Deposito_a_termine1)
	testData$Deposito_a_termine1 <- positionFactory(repository$Deposito_a_termine1)
	
	return(testData)
}
