# TODO: Add comment
# 
# Author: claudio
###############################################################################

# TODO: Add comment
#
# Author: claudio
###############################################################################

test.shouldCreateConto_corrente_fittizio <- function() {
	## uses a default method
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Conto_corrente_fittizio
	class(origin) <- "Ayrton_Conto_corrente_fittizio"
	
	ccFittizio <- createSecurity(origin)
	
	checkEquals(is(ccFittizio)[[1]],"Conto_corrente_fittizio")
	checkEquals(ccFittizio@name,"SMI Futures 21-09-2012 / 10")
	checkEquals(ccFittizio@currency,new("Currency","CHF"))
	checkEquals(ccFittizio@id,new("IdAyrton",idAAA=new("IdAAA_character","CHF-chf"),idStrumento=54))
	
}


test.shouldCreateUnclassified <- function() {
	## uses a default method
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$equity1
	class(origin) <- "Ayrton_Unclassified"
	
	unclassified <- createSecurity(origin)
	
	checkEquals(unclassified@name,"Roche Holding Gs")
	checkEquals(unclassified@currency,new("Currency","CHF"))
	checkEquals(unclassified@id,new("IdAyrton",idAAA=new("IdAAA_character","1203204CH"),idStrumento=1))
	
}


test.shouldCreateEquity <- function() {
	## uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the equity repository and instrument repository
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$equity1
	class(origin) <- "Ayrton_Equity"
	
	equity <- createSecurity(origin)
	
	checkEquals(equity@name,"Roche Holding Gs")
	checkEquals(equity@currency,new("Currency","CHF"))
	checkEquals(equity@id,new("IdAyrton",idAAA=new("IdAAA_character","1203204CH"),idStrumento=1))
	
	## same test but with equity in EUR
	origin <- repository$equity2
	class(origin) <- "Ayrton_Equity"
	
	equity <- createSecurity(origin)
	
	checkEquals(equity@currency,new("Currency","EUR"))
	
	
	## restore initial conditions
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
}

test.shouldCreateBond <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$bond1
	class(origin) <- "Ayrton_Bond"
	
	bond <- createSecurity(origin)
	
	checkEquals(bond@name,"20130603 - 3.625% Pfizer 03-06-13")
	checkEquals(bond@id,new("IdAyrton",idAAA=new("IdAAA_character","10234542"),idStrumento=2))
	checkEquals(bond@maturity,"2013-06-03")
	
}

test.shouldCreateFondi_obbligazionari <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$fondiObbligazionari
	class(origin) <- "Ayrton_Fondi_obbligazionari"
	
	fondo <- createSecurity(origin)
	
	checkEquals(fondo@name,"20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20")
	checkEquals(fondo@id,new("IdAyrton",idAAA=new("IdAAA_character","2490099"),idStrumento=3))
	checkEquals(fondo@maturity,"2020-12-31")
	
	# the NoAc version (funds without the Oacc information and therefore with Oacc = 0)
	origin <- repository$fondiObbligazionariNoAC
	class(origin) <- "Ayrton_Fondi_obbligazionari"
	
	fondo <- createSecurity(origin)
	
	checkEquals(fondo@name,"20201231 - 0% <3Y - LGT CF 2Y CHF 31-12-20")
	checkEquals(fondo@id,new("IdAyrton",idAAA=new("IdAAA_character","1831257"),idStrumento=3))
	checkEquals(fondo@maturity,"2020-12-31")

}


test.shouldCreateAnticipoFisso <- function() {
	## this is valid for Anticipi fissi
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Anticipi_fissi1
	class(origin) <- "Ayrton_Anticipi_fissi"
	
	anticipiFissi <- createSecurity(origin)
	checkEquals(anticipiFissi@name,"Anticipo fisso 01-04-09/02-04-12 Ipoteca tasso fisso 115.000 CHF 2.05%")
	checkEquals(anticipiFissi@id,new("IdAyrton",idAAA=new("IdAAA_character","Anticipo fisso 01-04-09/02-04-12 Ipoteca tasso fisso 115.000 CHF 2.05%"),idStrumento=6))
	checkEquals(anticipiFissi@maturity,"2012-04-02")
	
}


test.shouldCreateTimeDeposit <- function() {
	## this is valid for time deposits. The procedure is identical to that of Anticipi_fissi
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Deposito_a_termine1
	class(origin) <- "Ayrton_Depositi_a_termine"
	
	depositoAtermine <- createSecurity(origin)
	checkEquals(depositoAtermine@name,"Deposito singolo 01-04-09/02-04-12 deposito a termine al 2.05%")
	checkEquals(depositoAtermine@id,new("IdAyrton",idAAA=new("IdAAA_character","Deposito singolo 01-04-09/02-04-12 deposito a termine al 2.05%"),idStrumento=7))
	checkEquals(depositoAtermine@maturity,"2012-04-02")
	
}


test.shouldNULLForAccruedInterest <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the equity repository and instrument repository
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$proRata1
	class(origin) <- "Ayrton_Bond"
	
	null <- createSecurity(origin)
	
	checkEquals(is.null(null),TRUE)
	
}

test.shouldCreateStrutturati_FI <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the equity repository and instrument repository
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$strutturati_FI
	class(origin) <- "Ayrton_Strutturati_FI"
	
	security <- createSecurity(origin)
	
	checkEquals(security@expiryDate,"2013-05-21")
	checkEquals(security@underlyingHorizon,"<3Y")
	
}

test.shouldCreateOpzioni_su_azioni <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the DBEquity repository and instrument repository
	allocateTestRepositories("instruments")
	allocateTestRepositories("DBEquities")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Opzioni_su_azioni1
	class(origin) <- "Ayrton_Opzioni_su_azioni"
	
	security <- createSecurity(origin)
	checkEquals(class(security)[[1]],"Opzioni_su_azioni")
	checkEquals(security@name,"-1000 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90")
	checkEquals(security@currency,new("Currency","CHF"))
	checkEquals(security@expiryDate,"2012-02-17")
	checkEquals(security@strike,290)
	
}

test.shouldCreateOpzioni_su_divise <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the DBEquity repository and instrument repository
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Opzioni_su_divise1
	class(origin) <- "Ayrton_Opzioni_su_divise"
	
	security <- createSecurity(origin)
	checkEquals(class(security)[[1]],"Opzioni_su_divise")
	checkEquals(security@name,"PUT 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)")
	checkEquals(security@currency,new("Currency","USD"))
	checkEquals(security@expiryDate,"2012-08-17")
	checkEquals(security@strike,1.295)
	
}

test.shouldCreateFutures_EQ <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the equity repository and instrument repository
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Futures_EQ1
	class(origin) <- "Ayrton_Futures_EQ"
	
	security <- createSecurity(origin)
	checkEquals(class(security)[[1]],"Futures_EQ")
	checkEquals(security@name,"SMI Futures 16-03-2012 / 10              ")
	checkEquals(security@currency,new("Currency","CHF"))
	checkEquals(security@deliveryDate,"2012-03-16")
}


test.shouldCreateObbligazioni_convertibili <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Obbligazioni_convertibili
	class(origin) <- "Ayrton_Obbligazioni_convertibili"
	
	convertibleBond <- createSecurity(origin)
	
	checkEquals(convertibleBond@name,"20130329 - 4% CS 29-03-13")
	checkEquals(convertibleBond@id,new("IdAyrton",idAAA=new("IdAAA_character","CH0190462702"),idStrumento=11))
	checkEquals(convertibleBond@maturity,"2013-03-29")
	
}



