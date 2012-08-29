# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldCreateUnclassifiedPosition <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$unclassified1
	
	unclassified <- securityFactory(origin)
	
	position <- createPosition(unclassified,origin)
	
	checkEquals(position@id,position@id)	
	checkEquals(position@quantity,100.3)
	checkEquals(position@value,toMoney(new("Amount",123.55),new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldBeNullPositionFromAccruedInterest <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$proRata1
	
	security <- securityFactory(origin)
	
	position <- createPosition(security,origin)
	
	checkEquals(position,NULL)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionEquity <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$equity1
	
	equitySecurity <- securityFactory(origin)
	
	equityPosition <- createPosition(equitySecurity,origin)
	
	checkEquals(is(equityPosition)[1],"PositionEquity")
	checkEquals(equityPosition@id,equityPosition@security@id)	
	checkEquals(equityPosition@quantity,15)
	checkEquals(equityPosition@value,toMoney(new("Amount",88205),new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionFuture_EQ <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Futures_EQ1
	
	securityFuture_EQ <- securityFactory(origin)
	
	positionFutures_EQ <- createPosition(securityFuture_EQ,origin)
	
	checkEquals(is(positionFutures_EQ)[1],"PositionFutures_EQ")
	checkEquals(positionFutures_EQ@id,positionFutures_EQ@security@id)	
	checkEquals(positionFutures_EQ@quantity,-25)
	checkEquals(positionFutures_EQ@valueOnePoint,toMoney(10,"CHF"))
	checkEquals(positionFutures_EQ@value,toMoney(new("Amount",0),new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}





test.shouldCreatePositionBond <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$bond1
	
	bondSecurity <- securityFactory(origin)
	
	bondPosition <- createPosition(bondSecurity,origin)
	
	checkEquals(is(bondPosition)[1],"PositionBond")
	checkEquals(bondPosition@id,bondPosition@security@id)
	checkEquals(bondPosition@accruedInterest@amount,new("Amount",NA_real_))	
	checkEquals(bondPosition@quantity,new("NominalValue",amount=new("Amount",100000),currency=new("Currency","EUR")))
	checkEquals(bondPosition@value,repositories$exchangeRates$exchange(toMoney(124345.632268,"CHF"),new("Currency","EUR")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionFondiObbligazionari <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$fondiObbligazionariNoAC
	
	FundSecurity <- securityFactory(origin)
	
	FundPosition <- createPosition(FundSecurity,origin)
	
	checkEquals(is(FundPosition)[1],"PositionFondi_obbligazionari")
	checkEquals(FundPosition@id,FundPosition@security@id)
	checkEquals(FundPosition@accruedInterest@amount,new("Amount",0))	
	checkEquals(FundPosition@quantity,105)
	checkEquals(FundPosition@value,toMoney(227808,"CHF"))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionAnticipiFissi <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Anticipi_fissi1
	
	anticipiFissiSecurity <- securityFactory(origin)
	
	anticipiFissiPosition <- createPosition(anticipiFissiSecurity,origin)
	
	checkEquals(is(anticipiFissiPosition)[1],"PositionAnticipi_fissi")
	checkEquals(anticipiFissiPosition@id,anticipiFissiPosition@security@id)
	checkEquals(anticipiFissiPosition@accruedInterest@amount,new("Amount",NA_real_))	
	checkEquals(anticipiFissiPosition@quantity,new("NominalValue",new("Money",amount=new("Amount",-1),currency=new("Currency","CHF"))))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionDepositiAtermine <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Deposito_a_termine1
	
	depositoAtermine <- securityFactory(origin)
	
	depositoAtermine <- createPosition(depositoAtermine,origin)
	
	checkEquals(is(depositoAtermine)[1],"PositionDepositi_a_termine")
	checkEquals(depositoAtermine@id,depositoAtermine@security@id)
	checkEquals(depositoAtermine@accruedInterest@amount,new("Amount",NA_real_))	
	checkEquals(depositoAtermine@quantity,new("NominalValue",new("Money",amount=new("Amount",1),currency=new("Currency","CHF"))))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionFondi_misti<- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Fondi_misti
	
	fondo_misto <- securityFactory(origin)
	
	fondo_misto <- createPosition(fondo_misto,origin)
	
	checkEquals(is(fondo_misto)[1],"PositionFondi_misti")
	checkEquals(fondo_misto@id,fondo_misto@security@id)
	checkEquals(fondo_misto@equityPart,30)
	checkEquals(fondo_misto@bondPart,70)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldCreatePositionConto_corrente<- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Conto_corrente2
	
	cc <- securityFactory(origin)
	
	cc <- createPosition(cc,origin)
	
	checkEquals(is(cc)[1],"PositionConto_corrente")
	checkEquals(cc@id,cc@security@id)
	checkEquals(cc@quantity,1.0)
	checkEquals(cc@value,repositories$exchangeRates$exchange(toMoney(39416.1310068511,"CHF"),new("Currency","EUR")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}



test.shouldCreatePositionObbligazioni_convertibili <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	mySetwd()
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Obbligazioni_convertibili
	
	convertibleBondSecurity <- securityFactory(origin)
	
	convertibleBondSecurity <- createPosition(convertibleBondSecurity,origin)
	
	checkEquals(is(convertibleBondSecurity)[1],"PositionObbligazioni_convertibili")
	checkEquals(convertibleBondSecurity@id,convertibleBondSecurity@security@id)
	checkEquals(convertibleBondSecurity@accruedInterest@amount,new("Amount",NA_real_))	
	checkEquals(convertibleBondSecurity@quantity,new("NominalValue",amount=new("Amount",25000),currency=new("Currency","CHF")))
	checkEquals(convertibleBondSecurity@value,toMoney(26312.5,"CHF"))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}


test.shouldCreatePositionOpzioni_su_azioni <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	allocateTestRepositories("DBEquities")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Opzioni_su_azioni1
	
	securityEquityOption <- securityFactory(origin)
	
	positionEquityOption <- createPosition(securityEquityOption,origin)
	
	
	checkEquals(is(positionEquityOption)[1],"PositionOpzioni_su_azioni")
	checkEquals(positionEquityOption@id,positionEquityOption@security@id)
	checkEquals(positionEquityOption@numberEquities,-1000)	
	checkEquals(positionEquityOption@quantity,NA_real_)
	checkEquals(positionEquityOption@contractSize,NA_real_)
	checkEquals(positionEquityOption@value,toMoney(-5840,"CHF"))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("DBEquities")
}

test.shouldCreatePositionOpzioni_su_divise <- function() {	
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	origin <- repository$Opzioni_su_divise1
	class(origin) <- "Ayrton_Opzioni_su_divise"
	
	securityOpzioniSuDivise <- securityFactory(origin)
	
	positionOpzioniSuDivise <- createPosition(securityOpzioniSuDivise,origin)
	
	checkEquals(is(positionOpzioniSuDivise)[1],"PositionOpzioni_su_divise")
	checkEquals(positionOpzioniSuDivise@id,new("IdCharacter","PUT 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"))	
	checkEquals(positionOpzioniSuDivise@quantity,toMoney(125000,"EUR"))
	checkEquals(positionOpzioniSuDivise@value,repositories$exchangeRates$exchange(toMoney(origin@ValoreMercatoMonetaCHF,"CHF"),new("Currency",origin@Moneta)))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}


