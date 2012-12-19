# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryPositions <- function() {

	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the instrument repository	
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	
	testData <- new.env()

	## create equity1
	Moneta <- "CHF"
	Saldo <- 15
	NumeroValore <- "1203204CH"
	Nome <- "Roche Holding Gs"
	ValoreMercatoMonetaCHF <- 88205
	ID_strumento <- 1

	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Equity",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionEquity",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	A_equity1 <- x
	testData$equity1 <- x

	## create equity2
	Moneta <- "EUR"
	Saldo <- 1000
	NumeroValore <- "1469452EU"
	Nome <- "Kontron AG"
	ValoreMercatoMonetaCHF <- 7439.7503136
	ID_strumento <- 1
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Equity",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionEquity",
			id=id,
			security=security,
			quantity=quantity,
			value=value)	

	A_equity2 <- x
	testData$equity2 <- x
	
	## create an indexCertificate
	Moneta <- "USD"
	Saldo <- 100
	NumeroValore <- "US46429B3096"
	Nome <- "ISHARES MSCI Indon"
	ValoreMercatoMonetaCHF <- 283354.88
	ID_strumento <- 15
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Index_certificate",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)	
	
	A_indexCertificate <- x
	testData$indexCertificate <- x
	
	# ---------------------------------------
	# create bond1
	Moneta <- "EUR"
	Saldo <- 100000
	NumeroValore <- "10234542"
	Nome <- "20130603 - 3.625% Pfizer 03-06-13"
	ValoreMercatoMonetaCHF <- 124345.632268
	ID_AAA <- 1218
	ID_strumento <- 2
	rating <- "AAA"
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2013-06-03"

	security <- new("Bond",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- new("NominalValue",amount=new("Amount",Saldo),currency=currency)
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionBond",
			# accruedInterest= 
			rating=rating,
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_bond1 <- x
	testData$bond1 <- x
	
	
	# ---------------------------------------
	# create bond2
	Moneta <- "EUR"
	Saldo <- 300000
	NumeroValore <- "10334618M"
	Nome <- "20120410 - 1.503% EIB FRN 10-04-12"
	ValoreMercatoMonetaCHF <- 362217.41556
	ID_AAA <- 1976
	ID_strumento <- 2
	rating <- "B"
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2012-04-12"
	
	security <- new("Bond",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- new("NominalValue",amount=new("Amount",Saldo),currency=currency)
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionBond",
			# accruedInterest= 
			rating=rating,
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_bond2 <- x
	testData$bond2 <- x
	
	
	# ---------------------------------------
	# create bond3
	Moneta <- "EUR"
	Saldo <- 200000
	NumeroValore <- "11429971F"
	Nome <- "20120319 - 1.869% Rabobank Nederland 19-03-12"
	ValoreMercatoMonetaCHF <- 241164.668888
	ID_AAA <- 1967
	ID_strumento <- 2
	rating <- "C"
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2012-03-19"
	
	security <- new("Bond",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- new("NominalValue",amount=new("Amount",Saldo),currency=currency)
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionBond",
			# accruedInterest= 
			rating=rating,
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_bond3 <- x
	testData$bond3 <- x	
	
	
	# ---------------------------------------
	# create bond4
	Moneta <- "CHF"
	Saldo <- 50000
	NumeroValore <- "2403071"
	Nome <- "20120221 - 2% Toyota 21-02-12"
	ValoreMercatoMonetaCHF <- 50025
	ID_AAA <- 1073
	ID_strumento <- 2
	rating <- "BB"
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2012-02-21"
	
	security <- new("Bond",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- new("NominalValue",amount=new("Amount",Saldo),currency=currency)
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionBond",
			# accruedInterest= 
			rating=rating,
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_bond4 <- x
	testData$bond4 <- x		
	
	
	# ---------------------------------------
	# create bond_floater
	Moneta <- "EUR"
	Saldo <- 75000
	NumeroValore <- "XS0439139998"
	Nome <- "20130109 - 0.415% EIB FRN [09.01.2015] 09-01-13"
	ValoreMercatoMonetaCHF <- 90861.23
	ID_AAA <- 2242
	ID_strumento <- 56
	rating <- "AAA"
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2013-01-09"
	
	security <- new("Bond_floater",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- new("NominalValue",amount=new("Amount",Saldo),currency=currency)
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionBond",
			# accruedInterest= 
			rating=rating,
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_bond_floater <- x
	testData$bond_floater <- x	

	
	# ---------------------------------------
	# create Fondi_obbligazionari with 0 accruedInterest (because included in the price)
	Moneta <- "CHF"
	Saldo <- 105
	NumeroValore <- "1831257"
	Nome <- "20201231 - 0% <3Y - LGT CF 2Y CHF 31-12-20"
	ValoreMercatoMonetaCHF <- 227808
	ID_AAA <- 363
	ID_strumento <- 3
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2020-12-31"
	
	security <- new("Fondi_obbligazionari",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_obbligazionari",
			accruedInterest=new("AccruedInterest",toMoney(0.0,security@currency)),
			rating=longTermRatingFactory("NR"),
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_fondiObbligazionariNoAC <- x
	testData$fondiObbligazionariNoAC <- x	
	
	
	# ---------------------------------------
	# create Fondi_obbligazionari from OpenCapital (accruedInterest set to NA_real_)
	Moneta <- "EUR"
	Saldo <- 10000
	NumeroValore <- "LU0810451608"
	Nome <- "20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	ValoreMercatoMonetaCHF <- 227808
	ID_AAA <- 825
	ID_strumento <- 3
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2020-12-31"
	
	security <- new("Fondi_obbligazionari",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_obbligazionari",
			accruedInterest=new("AccruedInterest",toMoney(NA_real_,security@currency)),
			rating=longTermRatingFactory("NR"),
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_fondiObbligazionari <- x
	testData$fondiObbligazionari <- x
	
	

	# ---------------------------------------
	# create Strutturati_FI
	Moneta <- "EUR"
	Saldo <- 150000
	NumeroValore <- "11439214"
	Nome <- "20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS"
	ValoreMercatoMonetaCHF <- 179299.42998
	ID_AAA <- 98
	ID_strumento <- 49
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	expiryDate <- "2013-05-21"
	underlyingHorizon <- "<3Y"
	security <- new("Strutturati_FI",currency=currency,name=name,id=id,expiryDate=expiryDate,underlyingHorizon=underlyingHorizon)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionStrutturati_FI",,
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_strutturati_FI <- x
	testData$strutturati_FI <- x
	
	
	# ---------------------------------------
	# create Fondi_misti
	Moneta <- "CHF"
	Saldo <- 400
	NumeroValore <- "279211CH"
	Nome <- "70-30 UBS Strategy Fund Yield CHF"
	ValoreMercatoMonetaCHF <- 46988
	ID_AAA <- 1476
	ID_strumento <- 26
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	security <- new("Fondi_misti",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_misti",
			equityPart=30,
			bondPart=70,
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Fondi_misti <- x	
	testData$Fondi_misti <- x
	
	
	
	# ---------------------------------------
	# create OpenCapital fund "Fixed Income"
	Moneta <- "EUR"
	Saldo <- 11000
	NumeroValore <- "LU0810451608"
	Nome <- "CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	ValoreMercatoMonetaCHF <- 1686274.5866043
	ID_AAA <- 825
	ID_strumento <- 3
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	maturity <- "2020-12-31"
	
	security <- new("Fondi_obbligazionari",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_obbligazionari",
			accruedInterest=new("AccruedInterest",toMoney(NA_real_,security@currency)),
			rating=longTermRatingFactory("NR"),
			id=id,
			security=security,
			quantity=quantity,
			value=value)		

	A_fixedIncome <- x
	testData$fixedIncome <- x	
	
	
	
	# ---------------------------------------
	# create OpenCapital fund "Global Economy"
	Moneta <- "CHF"
	Saldo <- 100
	NumeroValore <- "11995588CH"
	Nome <- "CB-Accent Global Economy"
	ValoreMercatoMonetaCHF <- 9263
	ID_AAA <- 2256
	ID_strumento <- 14
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	underlyingHorizon <- "<3Y"
	security <- new("Fondi_azionari",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_azionari",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_globalEconomy  <- x
	testData$globalEconomy <- x
	
	
	
	# ---------------------------------------
	# create position  Fondi_mercato_monetario
	Moneta <- "USD"
	Saldo <- 9
	NumeroValore <- "1968421EU"
	Nome <- "20201231 - 0% Aberdeen Liquidity Fund 31-12-20"
	ValoreMercatoMonetaCHF <- 25246.17324864
	ID_AAA <- 162
	ID_strumento <- 4
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	security <- new("Fondi_mercato_monetario",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Fondi_mercato_monetario2  <- x
	testData$Fondi_mercato_monetario2 <- x

	
	
	# ---------------------------------------
	# create position on Floating_rate_notes
	Moneta <- "EUR"
	Saldo <- 65000
	NumeroValore <- "2122570B"
	Nome <- "20130307 - 0% Commezbk (variabile da 07-03-2007 a 07-03-2013)"
	ValoreMercatoMonetaCHF <- 77853.223734
	ID_AAA <- 1060
	ID_strumento <- 5
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	security <- new("Floating_rate_notes",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Floating_rate_notes2  <- x
	testData$Floating_rate_notes2 <- x
	
	
	
	
	# ---------------------------------------
	# create position on Anticipi_fissi
	Moneta <- "CHF"
	Saldo <- -1.0
	NumeroValore <- ""
	Nome <- "Anticipo fisso 01-04-09/02-04-12 Ipoteca tasso fisso 115.000 CHF 2.05%"
	ValoreMercatoMonetaCHF <- -1.0
	ID_AAA <- NA_real_
	ID_strumento <- 6
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	security <- new("Anticipi_fissi",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionAnticipi_fissi",
			accruedInterest=new("AccruedInterest",toMoney(NA_real_,security@currency)),
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Anticipi_fissi1  <- x
	testData$Anticipi_fissi1 <- x
	
	

	
	# ---------------------------------------
	# create position on Anticipi_fissi
	Moneta <- "CHF"
	Saldo <- -15.50
	NumeroValore <- ""
	Nome <- "Anticipo fisso 20-12-11/14-02-12 Novers 0.65%"
	ValoreMercatoMonetaCHF <- -15.50
	ID_AAA <- NA_real_
	ID_strumento <- 6
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	security <- new("Anticipi_fissi",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionAnticipi_fissi",
			accruedInterest=new("AccruedInterest",toMoney(NA_real_,security@currency)),
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Anticipi_fissi2  <- x
	testData$Anticipi_fissi2 <- x
	
	
	
	# ---------------------------------------
	# create position on Deposito_a_termine
	Moneta <- "CHF"
	Saldo <- 1
	NumeroValore <- ""
	Nome <- "Deposito singolo 01-04-09/02-04-12 deposito a termine al 2.05%"
	maturity <- "2012-04-02"
	ValoreMercatoMonetaCHF <- 1.0
	ID_AAA <- NA_real_
	ID_strumento <- 7
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	security <- new("Deposito_a_termine",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionDeposito_a_termine",
			accruedInterest=new("AccruedInterest",toMoney(NA_real_,security@currency)),
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Deposito_a_termine1  <- x
	testData$Deposito_a_termine1 <- x
	
	

	
	# ---------------------------------------
	# create Fondi_azionari
	Moneta <- "EUR"
	Saldo <- 157
	NumeroValore <- "1021903EU"
	Nome <- "Pictet SICAV Water P Cap"
	ValoreMercatoMonetaCHF <- 30388.376629908
	ID_AAA <- 1840
	ID_strumento <- 14
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Fondi_azionari",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_azionari",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Fondi_azionari1 <- x
	testData$Fondi_azionari1 <- x

	
	# ---------------------------------------
	# create Fondi_azionari
	Moneta <- "CHF"
	Saldo <- 250
	NumeroValore <- "LU0810451434"
	Nome <- "CB-Accent Global Equity Fund Cap B"
	ValoreMercatoMonetaCHF <- 17115
	ID_AAA <- 1701
	ID_strumento <- 14
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Fondi_azionari",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_azionari",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Fondi_azionari2 <- x	
	testData$Fondi_azionari2 <- x

	
	# ---------------------------------------
	# create OpenCapital fund "Global Equity"
	Moneta <- "CHF"
	Saldo <- 2000
	NumeroValore <- "2742261CH"
	Nome <- "CB-Accent Global Equity Fund Cap B"
	ValoreMercatoMonetaCHF <- 147480
	ID_AAA <- 1701
	ID_strumento <- 14
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Fondi_azionari",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFondi_azionari",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_globalEquity <- x	
	testData$globalEquity <- x	
	
	
	# ---------------------------------------
	# create Index_certificates
	Moneta <- "USD"
	Saldo <- 10000
	NumeroValore <- "US46429B3096"
	Nome <- "ISHARES MSCI Indon"
	ValoreMercatoMonetaCHF <- 283354.88
	ID_AAA <- 2272
	ID_strumento <- 15
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Index_certificates",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Index_certificate1 <- x
	testData$Index_certificate1 <- x
	
	
	# ---------------------------------------
	# create Index_certificates
	Moneta <- "USD"
	Saldo <- 10000
	NumeroValore <- "US78464A7972"
	Nome <- "SPDR KBW ETF"
	ValoreMercatoMonetaCHF <- 201963.76
	ID_AAA <- 2273
	ID_strumento <- 15
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Index_certificates",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Index_certificate2 <- x	
	testData$Index_certificate2 <- x

	
	# ---------------------------------------
	# create Opzioni_su_azioni
	Moneta <- "CHF"
	Saldo <- -100
	NumeroValore <- ""
	Nome <- "-100 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90 / 10"
	numberEquities <- -1000
	contractSize <- 10
	optionType <- "C"
	idAAA_character <- "C/CH0011027469/2012-02-17/290"
	underlying <- new("Equity",name="Equity not in DBEquity",currency=new("Currency",Moneta),id=new("IdCharacter","CH0011027469"))
	expiryDate <- "2012-02-17"
	strike <- 290
	ValoreMercatoMonetaCHF <- -5840
	ID_AAA <- NA_real_
	ID_strumento <- 18
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idAAA_character),idStrumento=ID_strumento)
	
	security <- new("Opzioni_su_azioni",currency=currency,name=name,id=id,optionType=optionType,underlying=underlying,expiry=expiry,strike=strike)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionOpzioni_su_azioni",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Opzioni_su_azioni1 <- x
	testData$Opzioni_su_azioni1 <- x

	
	# ---------------------------------------
	# create Opzioni_su_azioni
	Moneta <- "CHF"
	Saldo <- -500
	NumeroValore <- ""
	Nome <- "-500 / PUT / Credit Suisse Group Na / 21-12-12 / Strike 46 / Premio(112267 CHF) / CH0012138530 / 17.71 / 10"
	numberEquities <- -5000
	contractSize <- 10
	optionType <- "P"
	idAAA_character <- "P/CH0012138530/2012-12-21/46"
	underlying <- new("Equity",name="Credit Suisse Group Na",currency=new("Currency",Moneta),
			id=new("IdAyrton",idAAA=new("IdCharacter","CH0012138530"),idStrumento=1))
	expiryDate <- "2012-12-21"
	strike <- 46
	ValoreMercatoMonetaCHF <- -107100
	ID_AAA <- NA_real_
	ID_strumento <- 18
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idAAA_character),idStrumento=ID_strumento)
	
	security <- new("Opzioni_su_azioni",currency=currency,name=name,id=id,optionType=optionType,underlying=underlying,expiry=expiry,strike=strike)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionOpzioni_su_azioni",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		

	A_Opzioni_su_azioni2 <- x		
	testData$Opzioni_su_azioni2 <- x

	
	# ---------------------------------------
	# create Opzioni_su_azioni
	Moneta <- "CHF"
	Saldo <- 100
	NumeroValore <- ""
	Nome <- "100 / PUT / Syngenta AG / 17-02-12 / Strike 290 / Premio(-5500 CHF) / CH0011027469 / 337.90 / 10"
	numberEquities <- 1000
	contractSize <- 10
	optionType <- "P"
	idAAA_character <- "P/CH0011027469/2012-02-17/290"
	underlying <- new("Equity",name="Equity not in DBEquity",currency=new("Currency",Moneta),id=new("IdCharacter","CH0011027469"))
	expiryDate <- "2012-02-17"
	strike <- 290
	ValoreMercatoMonetaCHF <- 5840
	ID_AAA <- NA_real_
	ID_strumento <- 18
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idAAA_character),idStrumento=ID_strumento)
	
	security <- new("Opzioni_su_azioni",currency=currency,name=name,id=id,optionType=optionType,underlying=underlying,expiry=expiry,strike=strike)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionOpzioni_su_azioni",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Opzioni_su_azioni3 <- x
	testData$Opzioni_su_azioni3 <- x
	
	
	# ---------------------------------------
	# create Opzioni_su_azioni
	Moneta <- "CHF"
	Saldo <- 500
	NumeroValore <- ""
	Nome <- "500 / Call / Credit Suisse Group Na / 21-12-12 / Strike 46 / Premio(-112267 CHF) / CH0012138530 / 17.71 / 10"
	numberEquities <- 5000
	contractSize <- 10
	optionType <- "C"
	idAAA_character <- "C/CH0012138530/2012-12-21/46"
	underlying <- new("Equity",name="Credit Suisse Group Na",currency=new("Currency",Moneta),
			id=new("IdAyrton",idAAA=new("IdCharacter","CH0012138530"),idStrumento=1))
	expiryDate <- "2012-12-21"
	strike <- 46
	ValoreMercatoMonetaCHF <- 107100
	ID_AAA <- NA_real_
	ID_strumento <- 18
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idAAA_character),idStrumento=ID_strumento)
	
	security <- new("Opzioni_su_azioni",currency=currency,name=name,id=id,optionType=optionType,underlying=underlying,expiry=expiry,strike=strike)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionOpzioni_su_azioni",
			id=id,
			security=security,
			quantity=quantity,
			value=value)		
	
	A_Opzioni_su_azioni4 <- x
	testData$Opzioni_su_azioni4 <- x
	
	
	# ---------------------------------------
	# create Futures_EQ
	Moneta <- "CHF"
	Saldo <- -25
	NumeroValore <- ""
	Nome <- "Future SMI 16-03-2012 / 10"
	idAAA_character <- "Future SMI2012-03-16"
	underlying <- new("IndexEquity",name="Future SMI",id=new("IdCharacter","Future SMI"))
	
	PrezzoMercato <- 6500
	deliveryDate <- "2012-03-16"
	valueOnePoint <- 10
	
	ID_AAA <- NA_real_
	ID_strumento <- 50
	valoreMercatoMonetaCHF <- 0
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idAAA_character),idStrumento=ID_strumento)
	
	security <- new("Futures_EQ",currency=currency,name=name,id=id,underlying=underlying,deliveryDate=deliveryDate)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionFutures_EQ",
			indexLevel=PrezzoMercato,
			valueOnePoint=toMoney(valueOnePoint,currency),
			id=id,
			security=security,
			quantity=quantity,
			value=value)

	A_Futures_EQ1 <- x
	testData$Futures_EQ1 <- x
	
	
	
	# ---------------------------------------
	# create Metalli_preziosi
	Moneta <- "USD"
	Saldo <- 180
	NumeroValore <- ""
	Nome <- "XAU"
	idAAA_character <- "XAU"
	
	ID_strumento <- 21
	valoreMercatoMonetaCHF <- 284105.4768
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idAAA_character),idStrumento=ID_strumento)
	
	security <- new("Metalli_preziosi",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Metalli_preziosi1 <- x
	testData$Metalli_preziosi1 <- x

	
	# ---------------------------------------
	# create FX_Forward
	Moneta <- "CHF"
	Saldo <- -829060.081320845
	NumeroValore <- ""
	Nome <- "CHF -1,000,000.00 Valuta 26-03-2012"
	idAAA_character <- "CHF26-03-2012"
	
	ID_strumento <- 22
	valoreMercatoMonetaCHF <- -1e+06
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idAAA_character),idStrumento=ID_strumento)
	
	security <- new("FX_Forward",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_FX_Forward1 <- x
	testData$FX_Forward1 <- x

	
	# ---------------------------------------
	# create Fondi_immobiliari
	Moneta <- "EUR"
	Saldo <- 2298.4
	NumeroValore <- "1968401EU"
	Nome <- "UBS WM Global Property Fund EUR"
	ID_strumento <- 25
	valoreMercatoMonetaCHF <- 18225.6287818451
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Fondi_immobiliari",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Fondi_immobiliari1 <- x
	testData$Fondi_immobiliari1 <- x

	# ---------------------------------------
	# create Diritti_aumento_capitale_azionario
	Moneta <- "EUR"
	Saldo <- 29946
	NumeroValore <- "14742829CH"
	Nome <- "Rights on Banco Santander"
	ID_strumento <- 30
	valoreMercatoMonetaCHF <- 4623.4140158976
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Diritti_aumento_capitale_azionario",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Diritti_aumento_capitale_azionario1 <- x
	testData$Diritti_aumento_capitale_azionario1 <- x

	
	# ---------------------------------------
	# create Conto_corrente
	Moneta <- "CHF"
	Saldo <- 219365.039954224
	NumeroValore <- ""
	Nome <- "CHF-16.4105.2120.001.01"
	ID_strumento <- 40
	valoreMercatoMonetaCHF <- 219365.039954224
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character","CHF-chf"),idStrumento=ID_strumento)
	
	security <- new("Conto_corrente",currency=currency,name=name,id=id)
	quantity <- 1
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionConto_corrente",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Conto_corrente1 <- x
	testData$Conto_corrente1 <- x
	

	# ---------------------------------------	
	# create Fondi_Hedge
	Moneta <- "CHF"
	Saldo <-  0.29
	NumeroValore <- "10063727CH"
	Nome <- "GEMS PROGRESSIVE FD SICAV LOW VOLATILITY RESERVE POOL"
	ID_strumento <- 44
	valoreMercatoMonetaCHF <- 291.9517
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Fondi_Hedge",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Fondi_Hedge1 <- x
	testData$Fondi_Hedge1 <- x

	
	# ---------------------------------------	
	# create ETF_equity
	Moneta <- "EUR"
	Saldo <- 5100
	NumeroValore <- "FR0010346205"
	Nome <- "Lyxor ETF CRB"
	ID_strumento <- 45
	valoreMercatoMonetaCHF <- 134472.7632072
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("ETF_equity",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_ETF_equity1 <- x
	testData$ETF_equity1 <- x

	
	# ---------------------------------------	
	# create Strutturati_EQ
	Moneta <- "USD"
	Saldo <- 5e+05
	NumeroValore <- "FR0010429068"
	Nome <- "Certificat EMI Emerging Market Momentum"
	ID_strumento <- 48
	valoreMercatoMonetaCHF <- 350385.56
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Strutturati_EQ",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)

	
	A_Strutturati_EQ1 <- x	
	testData$Strutturati_EQ1 <- x
	
	
	# ---------------------------------------	
	# create Strutturati_FI
	Moneta <- "EUR"
	Saldo <- 150000
	NumeroValore <- "11439214"
	Nome <- "20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS"
	expiryDate <- "2013-05-21"
	underlyingHorizon <- "<3Y"
	ID_strumento <- 49
	valoreMercatoMonetaCHF <- 179299.42998
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Strutturati_FI",currency=currency,name=name,id=id,
			expiryDate=expiryDate,underlyingHorizon=underlyingHorizon)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	

	A_Strutturati_FI1 <- x	
	testData$Strutturati_FI1 <- x

	
	# ---------------------------------------	
	# create ETF_commodities_gold
	Moneta <- "USD"
	Saldo <- 3000
	NumeroValore <- "GB00B00FHZ82"
	Nome <- "Gold Bullion Securities"
	ID_strumento <- 51
	valoreMercatoMonetaCHF <- 434745.41
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("ETF_commodities_gold",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_ETF_commodities_gold <- x
	testData$ETF_commodities_gold <- x
	
	
	# ---------------------------------------	
	# create ETF_commodities_platinum
	Moneta <- "USD"
	Saldo <- 276
	NumeroValore <- "CH0116014934"
	Nome <- "IS Platinum ETF"
	ID_strumento <- 55
	valoreMercatoMonetaCHF <- 37192.09
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("ETF_commodities_platinum",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_ETF_commodities_platinum <- x
	testData$ETF_commodities_platinum <- x
	
	
	# ---------------------------------------	
	# create Credit_linked_note
	Moneta <- "EUR"
	Saldo <- 500000
	NumeroValore <- "14723815"
	Nome <- "20130404 - Credit-linked Note UBS Jersey (Credit of Air France)"
	ID_strumento <- 52
	valoreMercatoMonetaCHF <- 600743.26
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Credit_linked_note",currency=currency,name=name,id=id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	A_Credit_linked_note <- x
	testData$Credit_linked_note <- x
	
	
	# ---------------------------------------	
	# create Obbligazioni_convertibili
	Moneta <- "CHF"
	Saldo <- 25000
	NumeroValore <- "CH0190462702"
	Nome <- "20130329 - 4% CS 29-03-13"
	ID_strumento <- 11
	valoreMercatoMonetaCHF <- 26312.5

	maturity <- "2013-03-29"
	rating <- longTermRatingFactory("BB")
	
	emptyEquity <- new("Equity",id=new("IdCharacter",NA_character_))
	emptyId <- new("IdCharacter",NA_character_)
	optionSecurity <- new("Opzioni_su_azioni",currency=new("Currency",Moneta),optionType="C",
			id=emptyId,underlying=emptyEquity)
	positionCall <- new("PositionOpzioni_su_azioni",numberEquities=0,contractSize=0,
			quantity=0,value=toMoney(0,Moneta),security=optionSecurity,id=emptyId)
	
	accruedInterest <- new("AccruedInterest",toMoney(NA_real_,Moneta))
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Obbligazioni_convertibili",currency=currency,name=name,id=id,maturity=maturity)
	quantity <- new("NominalValue",amount=new("Amount",Saldo),currency=currency)
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionObbligazioni_convertibili",
			id=security@id,
			security=security,
			quantity=quantity,
			value=value,
			rating=rating,
			positionCall=positionCall,
			accruedInterest=accruedInterest)
	
	
	A_Obbligazioni_convertibili <- x
	testData$Obbligazioni_convertibili <- x




	# ---------------------------------------
	# create Conto_corrente_fittizio
	Moneta <- "CHF"
	Saldo <- 4590600
	NumeroValore <- ""
	Nome <- "Future SMI 21-09-2012 / 10"
	ID_strumento <- 54
	valoreMercatoMonetaCHF <- 4590600
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",idAAA=new("IdAAA_character","CHF-chf"),idStrumento=ID_strumento)
	
	security <- new("Conto_corrente_fittizio",currency=currency,name=name,id=id)
	quantity <- 1
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionConto_corrente_fittizio",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	
	
	A_Conto_corrente_fittizio <- x
	testData$Conto_corrente_fittizio <- x
	
	
}
