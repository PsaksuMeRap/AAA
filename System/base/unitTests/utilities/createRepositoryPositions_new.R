# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryPositions <- function() {

	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	
	testData <- new.env()

	# create a valid equity
	currency <- new("Currency","CHF")
	name <- "Roche Holding Gs"
	id <- new("IdAyrton",new("IdAAA_character","1203204CH"),idStrumento=1)
	
	x <- new("Equity",currency=currency,name=name,id)
		
	x <- new("positionEquity",
	equity1 <- positionFactory(repository$equity1)
	testData$equity1 <- equity1
	
	# create a valid equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo160"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 15
	x@NumeroValore <- "1203204CH"
	x@Nome <- 
	x@ValoreMercatoMonetaCHF <- 88205
	x@ID_AAA <- 824
	x@ID_strumento <- 1
	
	class(x) <- "AyrtonPosition"
	A_equity1 <- x
	testData$equity1 <- x
	
	
	
	
	# create a valid equity
	equity2 <- positionFactory(repository$equity2)
	testData$equity2 <- equity2
	
	# create a valid index certificate
	indexCertificate <- positionFactory(repository$indexCertificate)
	testData$indexCertificate <- indexCertificate
	
	# create a non existing equity
	noExists <- positionFactory(repository$noExists)
	testData$noExists <- noExists
	
	# create a valid bond
	bond1 <- positionFactory(repository$bond1)
	testData$bond1 <- bond1
	
	# create a valid bond
	bond2 <- positionFactory(repository$bond2)
	testData$bond2 <- bond2
	
	# create a valid bond
	bond3 <- positionFactory(repository$bond3)
	testData$bond3 <- bond3
	
	# create a valid bond (match to proRata1)
	bond4 <- positionFactory(repository$bond4)
	testData$bond4 <- bond4

	# create a valid AccruedInterest di Fondi_obbligazionari con 0 AC
	fondiObbligazionariNoAC <- positionFactory(repository$fondiObbligazionariNoAC)
	testData$fondiObbligazionariNoAC <- fondiObbligazionariNoAC
	
	# create a valid AccruedInterest di Fondi_obbligazionari
	fondiObbligazionari <- positionFactory(repository$fondiObbligazionari)
	testData$fondiObbligazionari <- fondiObbligazionari
	
	# create a valid AccruedInterest di Fondi_obbligazionari
	proRataFondiObbligazionari <- positionFactory(repository$proRataFondiObbligazionari)
	testData$proRataFondiObbligazionari <- proRataFondiObbligazionari
	
	# create a valid AccruedInterest
	proRata1 <- positionFactory(repository$proRata1)
	testData$proRata1 <- proRata1
	
	# create an Unclassified security
	unclassified1 <- positionFactory(repository$unclassified1)
	testData$unclassified1 <- unclassified1
	
	# create a strutturati_FI 
	strutturati_FI <- positionFactory(repository$strutturati_FI)
	testData$strutturati_FI <- strutturati_FI
	
	# create a Fondi_misti 
	Fondi_misti <- positionFactory(repository$Fondi_misti)
	testData$Fondi_misti <- Fondi_misti
	
	# create a globalEquity 
	globalEquity <- positionFactory(repository$globalEquity)
	testData$globalEquity <- globalEquity
	
	# create a globalEconomy 
	globalEconomy <- positionFactory(repository$globalEconomy)
	testData$globalEconomy <- globalEconomy
	
	# create a fixedIncome 
	fixedIncome <- positionFactory(repository$fixedIncome)
	testData$fixedIncome <- fixedIncome
	
	Fondi_mercato_monetario1 <- positionFactory(repository$Fondi_mercato_monetario1)
	testData$Fondi_mercato_monetario1 <- Fondi_mercato_monetario1
	
	Fondi_mercato_monetario2 <- positionFactory(repository$Fondi_mercato_monetario2)
	testData$Fondi_mercato_monetario2 <- Fondi_mercato_monetario2
	
	Floating_rate_notes1 <- positionFactory(repository$Floating_rate_notes1)
	testData$Floating_rate_notes1 <- Floating_rate_notes1
	
	Floating_rate_notes2 <- positionFactory(repository$Floating_rate_notes2)
	testData$Floating_rate_notes2 <- Floating_rate_notes2
	
	Anticipi_fissiAccrual1 <- positionFactory(repository$Anticipi_fissiAccrual1)
	testData$Anticipi_fissiAccrual1 <- Anticipi_fissiAccrual1
	
	Anticipi_fissi1 <- positionFactory(repository$Anticipi_fissi1)
	testData$Anticipi_fissi1 <- Anticipi_fissi1
	
	Anticipi_fissiAccrual2 <- positionFactory(repository$Anticipi_fissiAccrual2)
	testData$Anticipi_fissiAccrual2 <- Anticipi_fissiAccrual2
	
	Anticipi_fissi2 <- positionFactory(repository$Anticipi_fissi2)
	testData$Anticipi_fissi2 <- Anticipi_fissi2
	
	Fondi_azionari1 <- positionFactory(repository$Fondi_azionari1)
	testData$Fondi_azionari1 <- Fondi_azionari1
	
	Fondi_azionari2 <- positionFactory(repository$Fondi_azionari2)
	testData$Fondi_azionari2 <- Fondi_azionari2
	
	Index_certificate1 <- positionFactory(repository$Index_certificate1)
	testData$Index_certificate1 <- Index_certificate1
	
	Index_certificate2 <- positionFactory(repository$Index_certificate2)
	testData$Index_certificate2 <- Index_certificate2
	
	Opzioni_su_azioni1 <- positionFactory(repository$Opzioni_su_azioni1)
	testData$Opzioni_su_azioni1 <- Opzioni_su_azioni1
	
	Opzioni_su_azioni2 <- positionFactory(repository$Opzioni_su_azioni2)
	testData$Opzioni_su_azioni2 <- Opzioni_su_azioni2

	Opzioni_su_azioni3 <- positionFactory(repository$Opzioni_su_azioni3)
	testData$Opzioni_su_azioni3 <- Opzioni_su_azioni3
	
	Opzioni_su_azioni4 <- positionFactory(repository$Opzioni_su_azioni4)
	testData$Opzioni_su_azioni4 <- Opzioni_su_azioni4
	
	Deposito_a_termine_acc1 <- positionFactory(repository$Deposito_a_termine_acc1)
	testData$Deposito_a_termine_acc1 <- Deposito_a_termine_acc1	
	
	Deposito_a_termine1 <- positionFactory(repository$Deposito_a_termine1)
	testData$Deposito_a_termine1 <- Deposito_a_termine1
	
	Obbligazioni_convertibili <- positionFactory(repository$Obbligazioni_convertibili)
	testData$Obbligazioni_convertibili <- Obbligazioni_convertibili	
	
	proRataObbligazioni_convertibili <- positionFactory(repository$proRataObbligazioni_convertibili)
	testData$proRataObbligazioni_convertibili <- proRataObbligazioni_convertibili
	
	Futures_EQ1 <- positionFactory(repository$Futures_EQ1)
	testData$Futures_EQ1 <- Futures_EQ1
	
	Opzioni_su_divise1 <- positionFactory(repository$Opzioni_su_divise1)
	testData$Opzioni_su_divise1 <- Opzioni_su_divise1
	
	Opzioni_su_divise2 <- positionFactory(repository$Opzioni_su_divise2)
	testData$Opzioni_su_divise2 <- Opzioni_su_divise2

	Opzioni_su_divise3 <- positionFactory(repository$Opzioni_su_divise3)
	testData$Opzioni_su_divise3 <- Opzioni_su_divise3
	
	Opzioni_su_divise4 <- positionFactory(repository$Opzioni_su_divise4)
	testData$Opzioni_su_divise4 <- Opzioni_su_divise4
	
	Conto_corrente1 <- positionFactory(repository$Conto_corrente1)
	testData$Conto_corrente1 <- Conto_corrente1
	
	Conto_corrente_fittizio <- positionFactory(repository$Conto_corrente_fittizio)
	testData$Conto_corrente_fittizio <- Conto_corrente_fittizio
	
	FX_forward <- positionFactory(repository$FX_Forward1)
	testData$FX_forward <- FX_forward
	
	return(testData)
}
