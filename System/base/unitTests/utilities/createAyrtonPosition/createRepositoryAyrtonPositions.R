createAyrtonAnticipi_fissi1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo47'
	x@Strumento <- 'M'
	x@Moneta <- 'CHF'
	x@Saldo <- -1.0
	x@NumeroValore <- ""
	x@Nome <- 'Anticipo fisso 01-04-09/02-04-12 Ipoteca tasso fisso 115.000 CHF 2.05%'
	x@ValoreMercatoMonetaCHF <- -1.0
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 6
	class(x) <- "AyrtonPosition"
}



createAyrtonAnticipi_fissi2  <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo131'
	x@Strumento <- 'M'
	x@Moneta <- 'CHF'
	x@Saldo <- -15.50
	x@NumeroValore <- ""
	x@Nome <- 'Anticipo fisso 20-12-11/14-02-12 Novers 0.65%'
	x@ValoreMercatoMonetaCHF <- -15.50
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 6
	class(x) <- "AyrtonPosition"
}



createAyrtonAnticipi_fissiAccrual1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo47'
	x@Strumento <- 'Macc'
	x@Moneta <- 'CHF'
	x@Saldo <- -1.0
	x@NumeroValore <- ""
	x@Nome <- 'Anticipo fisso 01-04-09/02-04-12 Ipoteca tasso fisso 115.000 CHF 2.05% Pro-rata'
	x@ValoreMercatoMonetaCHF <- -0.01
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 6
	class(x) <- "AyrtonPosition"
}



createAyrtonAnticipi_fissiAccrual2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo131'
	x@Strumento <- 'Macc'
	x@Moneta <- 'CHF'
	x@Saldo <- -15.50
	x@NumeroValore <- ""
	x@Nome <- 'Anticipo fisso 20-12-11/14-02-12 Novers 0.65% Pro-rata'
	x@ValoreMercatoMonetaCHF <- -0.15
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 6
	class(x) <- "AyrtonPosition"
}



createAyrtonBond1 <- function() {
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 100000
	x@NumeroValore <- "10234542"
	x@Nome <- "20130603 - 3.625% Pfizer 03-06-13"
	x@ValoreMercatoMonetaCHF <- 124345.632268
	x@ID_AAA <- 1218
	x@ID_strumento <- 2
	x@rating <- "AAA"
	class(x) <- "AyrtonPosition"
}



createAyrtonBond2 <- function() {
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 300000
	x@NumeroValore <- "10334618M"
	x@Nome <- "20120410 - 1.503% EIB FRN 10-04-12"
	x@ValoreMercatoMonetaCHF <- 362217.41556
	x@ID_AAA <- 1976
	x@ID_strumento <- 2
	x@rating <- "B"
	class(x) <- "AyrtonPosition"
}



createAyrtonBond3 <- function() {
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 200000
	x@NumeroValore <- "11429971F"
	x@Nome <- "20120319 - 1.869% Rabobank Nederland 19-03-12"
	x@ValoreMercatoMonetaCHF <- 241164.668888
	x@ID_AAA <- 1967
	x@ID_strumento <- 2
	x@rating <- "C"
	class(x) <- "AyrtonPosition"
}



createAyrtonBond4 <- function() {
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- "O      "
	x@Moneta <- "CHF"
	x@Saldo <- 50000
	x@NumeroValore <- "2403071"
	x@Nome <- "20120221 - 2% Toyota 21-02-12"
	x@ValoreMercatoMonetaCHF <- 50025
	x@ID_AAA <- 1073
	x@ID_strumento <- 2
	x@rating <- "BB"
	class(x) <- "AyrtonPosition"
}



createAyrtonConto_corrente_fittizio <- function() {
	# create a valid Conto_corrente_fittizio
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "L"
	x@Moneta <- "CHF"
	x@Saldo <- 4590600
	x@NumeroValore <- ""
	x@Nome <- "Future SMI 21-09-2012 / 10"
	x@ValoreMercatoMonetaCHF <- 4590600
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 54
	class(x) <- "AyrtonPosition"
}


createAyrtonConto_corrente1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo11'
	x@Strumento <- 'L'
	x@Moneta <- 'CHF'
	x@Saldo <- 219365.039954224
	x@NumeroValore <- ""
	x@Nome <- 'CHF-16.4105.2120.001.01'
	x@ValoreMercatoMonetaCHF <- 219365.039954224
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 40
	class(x) <- "AyrtonPosition"
}



createAyrtonConto_corrente2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo11'
	x@Strumento <- 'L'
	x@Moneta <- 'EUR'
	x@Saldo <- 32678.3407778931
	x@NumeroValore <- ""
	x@Nome <- 'EUR-16.4105.2120.814.01'
	x@ValoreMercatoMonetaCHF <- 39416.1310068511
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 40
	class(x) <- "AyrtonPosition"
}



createAyrtonCredit_linked_note <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo202'
	x@Strumento <- 'PS'
	x@Moneta <- 'EUR'
	x@Saldo <- 500000
	x@NumeroValore <- "14723815"
	x@Nome <- '20130404 - Credit-linked Note UBS Jersey (Credit of Air France)'
	x@ValoreMercatoMonetaCHF <- 600743.26
	x@ID_AAA <- 169
	x@ID_strumento <- 52
	class(x) <- "AyrtonPosition"
}



createAyrtonDeposito_a_termine_acc1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo47'
	x@Strumento <- 'Macc'
	x@Moneta <- 'CHF'
	x@Saldo <- 1.0
	x@NumeroValore <- ""
	x@Nome <- 'Deposito singolo 01-04-09/02-04-12 deposito a termine al 2.05% Pro-rata'
	x@ValoreMercatoMonetaCHF <- 0.75
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 7
	class(x) <- "AyrtonPosition"
	
}



createAyrtonDeposito_a_termine1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo47'
	x@Strumento <- 'M'
	x@Moneta <- 'CHF'
	x@Saldo <- 1.0
	x@NumeroValore <- ""
	x@Nome <- 'Deposito singolo 01-04-09/02-04-12 deposito a termine al 2.05%'
	x@ValoreMercatoMonetaCHF <- 1.0
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 7
	class(x) <- "AyrtonPosition"
}



createAyrtonDiritti_aumento_capitale_azionario1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo186'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 29946
	x@NumeroValore <- "14742829CH"
	x@Nome <- 'Rights on Banco Santander'
	x@ValoreMercatoMonetaCHF <- 4623.4140158976
	x@ID_AAA <- 2422
	x@ID_strumento <- 30
	class(x) <- "AyrtonPosition"
}



createAyrtonEquity1 <- function() {
	
	# create a valid equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo160"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 15
	x@NumeroValore <- "1203204CH"
	x@Nome <- "Roche Holding Gs"
	x@ValoreMercatoMonetaCHF <- 88205
	x@ID_AAA <- 824
	x@ID_strumento <- 1
	
	class(x) <- "AyrtonPosition"
	return(x)
}



createAyrtonEquity2 <- function() {
	# create a valid equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "A"
	x@Moneta <- "EUR"
	x@Saldo <- 1000
	x@NumeroValore <- "1469452EU"
	x@Nome <- "Kontron AG"
	x@ValoreMercatoMonetaCHF <- 7439.7503136
	x@ID_AAA <- 1772
	x@ID_strumento <- 1
	
	class(x) <- "AyrtonPosition"
}



createAyrtonETF_commodities_gold <- function() {
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo202'
	x@Strumento <- 'A'
	x@Moneta <- 'USD'
	x@Saldo <- 3000
	x@NumeroValore <- "GB00B00FHZ82"
	x@Nome <- "Gold Bullion Securities"
	x@ValoreMercatoMonetaCHF <- 434745.41
	x@ID_AAA <- 2430
	x@ID_strumento <- 51
	class(x) <- "AyrtonPosition"
	
}



createAyrtonETF_commodities_platinum <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo202'
	x@Strumento <- 'A'
	x@Moneta <- 'USD'
	x@Saldo <- 276
	x@NumeroValore <- "CH0116014934"
	x@Nome <- "IS Platinum ETF"
	x@ValoreMercatoMonetaCHF <- 37192.09
	x@ID_AAA <- 2461
	x@ID_strumento <- 55
	class(x) <- "AyrtonPosition"
}



createAyrtonETF_equity1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo9'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 5100
	x@NumeroValore <- "FR0010346205"
	x@Nome <- 'Lyxor ETF CRB'
	x@ValoreMercatoMonetaCHF <- 134472.7632072
	x@ID_AAA <- 2277
	x@ID_strumento <- 45
	class(x) <- "AyrtonPosition"
}



createAyrtonETF_equity2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo9'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 45000
	x@NumeroValore <- "FR0010429068"
	x@Nome <- 'Lyxor ETF MSCI Em Mkts'
	x@ValoreMercatoMonetaCHF <- 428690.281932
	x@ID_AAA <- 2278
	x@ID_strumento <- 45
	class(x) <- "AyrtonPosition"
}



createAyrtonFixedIncome <- function() {
	# create a fixedIncome position
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo16"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 11000
	x@NumeroValore <- "2490099"
	x@Nome <- "CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	x@ValoreMercatoMonetaCHF <- 1686274.5866043
	x@ID_AAA <- 825
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
}



createAyrtonFloating_rate_notes1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo75'
	x@Strumento <- 'Oacc'
	x@Moneta <- 'EUR'
	x@Saldo <- 65000
	x@NumeroValore <- "2122570B"
	x@Nome <- '20130307 - 0% Commezbk (variabile da 07-03-2007 a 07-03-2013) 07-03-13 Pro-rata'
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- 1060
	x@ID_strumento <- 5
	class(x) <- "AyrtonPosition"
}



createAyrtonFloating_rate_notes2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo75'
	x@Strumento <- 'O      '
	x@Moneta <- 'EUR'
	x@Saldo <- 65000
	x@NumeroValore <- "2122570B"
	x@Nome <- '20130307 - 0% Commezbk (variabile da 07-03-2007 a 07-03-2013) 07-03-13'
	x@ValoreMercatoMonetaCHF <- 77853.223734
	x@ID_AAA <- 1060
	x@ID_strumento <- 5
	class(x) <- "AyrtonPosition"
}



createAyrtonFondi_azionari1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo11'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 157
	x@NumeroValore <- "1021903EU"
	x@Nome <- 'Pictet SICAV Water P Cap'
	x@ValoreMercatoMonetaCHF <- 30388.376629908
	x@ID_AAA <- 1840
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"
}



createAyrtonFondi_azionari2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo14'
	x@Strumento <- 'A'
	x@Moneta <- 'CHF'
	x@Saldo <- 250
	x@NumeroValore <- "2742261CH"
	x@Nome <- 'CB-Accent Global Equity Fund Cap B'
	x@ValoreMercatoMonetaCHF <- 17115
	x@ID_AAA <- 1701
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"
}



createAyrtonFondi_Hedge1 <- function() {
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo72'
	x@Strumento <- 'A'
	x@Moneta <- 'CHF'
	x@Saldo <- 0.29
	x@NumeroValore <- "10063727CH"
	x@Nome <- 'GEMS PROGRESSIVE FD SICAV LOW VOLATILITY RESERVE POOL'
	x@ValoreMercatoMonetaCHF <- 291.9517
	x@ID_AAA <- 1869
	x@ID_strumento <- 44
	class(x) <- "AyrtonPosition"
	
}



createAyrtonFondi_Hedge2 <- function() {
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo192'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 1844.31
	x@NumeroValore <- "VGG7558X1924"
	x@Nome <- 'Signet Credit I EUR Class Segregated Portfolio'
	x@ValoreMercatoMonetaCHF <- 259474.944273368
	x@ID_AAA <- 1953
	x@ID_strumento <- 44
	class(x) <- "AyrtonPosition"
	
}



createAyrtonFondi_immobiliari1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo71'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 2298.4
	x@NumeroValore <- "1968401EU"
	x@Nome <- 'UBS WM Global Property Fund EUR'
	x@ValoreMercatoMonetaCHF <- 18225.6287818451
	x@ID_AAA <- 1884
	x@ID_strumento <- 25
	class(x) <- "AyrtonPosition"
	
}



createAyrtonFondi_mercato_monetario1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo88'
	x@Strumento <- 'Oacc'
	x@Moneta <- 'EUR'
	x@Saldo <- 48
	x@NumeroValore <- "1968401EU"
	x@Nome <- '20201231 - 0% UBS MM EUR 31-12-20 Pro-rata'
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- 469
	x@ID_strumento <- 4
	class(x) <- "AyrtonPosition"
}



createAyrtonFondi_mercato_monetario2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo165'
	x@Strumento <- 'O      '
	x@Moneta <- 'USD'
	x@Saldo <- 9
	x@NumeroValore <- "1968421EU"
	x@Nome <- '20201231 - 0% Aberdeen Liquidity Fund 31-12-20'
	x@ValoreMercatoMonetaCHF <- 25246.17324864
	x@ID_AAA <- 162
	x@ID_strumento <- 4
	class(x) <- "AyrtonPosition"
}



createAyrtonFondi_misti <- function() {
	# create a Fondi_misti
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo84"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 400
	x@NumeroValore <- "279211CH"
	x@Nome <- "70-30 UBS Strategy Fund Yield CHF"
	x@ValoreMercatoMonetaCHF <- 46988
	x@ID_AAA <- 1476
	x@ID_strumento <- 26
	class(x) <- "AyrtonPosition"	
}



createAyrtonFondi_obbligazionari <- function() {
	# create a valid  Fondi_obbligazionari
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 10000
	x@NumeroValore <- "2490099"
	x@Nome <- "20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	x@ValoreMercatoMonetaCHF <- 10300
	x@ID_AAA <- 825
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
}



createAyrtonFondi_obbligazionariNoAc <- function() {	
	# create a valid Fondi_obbligazionari con accruedInterst 0 
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo172"
	x@Strumento <- "O      "
	x@Moneta <- "CHF"
	x@Saldo <- 105
	x@NumeroValore <- "1831257"
	x@Nome <- "20201231 - 0% <3Y - LGT CF 2Y CHF 31-12-20"
	x@ValoreMercatoMonetaCHF <- 227808
	x@ID_AAA <- 363
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
}



createAyrtonFutures_EQ1 <- function() {
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo210'
	x@Strumento <- 'F'
	x@Moneta <- 'CHF'
	x@Saldo <- -25
	x@NumeroValore <- ""
	x@PrezzoMercato <- 6500
	x@Nome <- "Future SMI 16-03-2012 / 10              "
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 50
	class(x) <- "AyrtonPosition"
	
}



createAyrtonFX_Forward1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo16'
	x@Strumento <- 'FX'
	x@Moneta <- 'CHF'
	x@Saldo <- -829060.081320845
	x@NumeroValore <- ""
	x@Nome <- "CHF -1
			000
			000.00 Valuta 26-03-2012"
	x@ValoreMercatoMonetaCHF <- -1e+06
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 22
	class(x) <- "AyrtonPosition"
}



createAyrtonGlobalEconomy <- function() {
	# create a globalEconomy position
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo51"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 100
	x@NumeroValore <- "11995588CH"
	x@Nome <- "CB-Accent Global Economy"
	x@ValoreMercatoMonetaCHF <- 9263
	x@ID_AAA <- 2256
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"
}



createAyrtonGlobalEquity <- function() {
	# create a globalEquity position
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo123"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 2000
	x@NumeroValore <- "2742261CH"
	x@Nome <- "CB-Accent Global Equity Fund Cap B"
	x@ValoreMercatoMonetaCHF <- 147480
	x@ID_AAA <- 1701
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"	
}



createAyrtonIndex_certificate <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo210'
	x@Strumento <- 'A'
	x@Moneta <- 'USD'
	x@Saldo <- 10000
	x@Nome <- 'ISHARES MSCI Indon'
	x@NumeroValore <- "US46429B3096"
	x@ValoreMercatoMonetaCHF <- 283354.88
	x@ID_AAA <- 2272
	x@ID_strumento <- 15
	class(x) <- "AyrtonPosition"
}



createAyrtonIndex_certificate2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo210'
	x@Strumento <- 'A'
	x@Moneta <- 'USD'
	x@Saldo <- 10000
	x@Nome <- 'SPDR KBW ETF'
	x@NumeroValore <- "US78464A7972"
	x@ValoreMercatoMonetaCHF <- 201963.76
	x@ID_AAA <- 2273
	x@ID_strumento <- 15
	class(x) <- "AyrtonPosition"
}



createAyrtonIndexCertificate <- function() {
	# create a valid indexCertificate
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "A"
	x@Moneta <- "USD"
	x@Saldo <- 100
	x@NumeroValore <- "US46429B3096"
	x@Nome <- "ISHARES MSCI Indon"
	x@ValoreMercatoMonetaCHF <- 283354.88
	x@ID_AAA <- 283354.88
	x@ID_strumento <- 15
	
	class(x) <- "AyrtonPosition"
}



createAyrtonMetalli_preziosi1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo15'
	x@Strumento <- 'XAU'
	x@Moneta <- 'USD'
	x@Saldo <- 180
	x@NumeroValore <- "0" # look at Strumento
	x@Nome <- 'XAU'	
	x@ValoreMercatoMonetaCHF <- 284105.4768
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 21
	class(x) <- "AyrtonPosition"
}



createAyrtonNoExists <- function() {
	# create a non existing equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo160"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 19
	x@NumeroValore <- ""
	x@Nome <- "Non existing"
	x@ValoreMercatoMonetaCHF <- 88205
	x@ID_AAA <- 100020202
	x@ID_strumento <- 1
	class(x) <- "AyrtonPosition"
	A_noExists <- x	
	testData$noExists <- x
}



createAyrtonObbligazioni_convertibili <- function() {
	# create a valid Obbligazioni_convertibili
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "CHF"
	x@Saldo <- 25000
	x@NumeroValore <- "CH0190462702"
	x@Nome <- "20130329 - 4% CS 29-03-13"
	x@ValoreMercatoMonetaCHF <- 26312.5
	x@ID_AAA <- 2209
	x@ID_strumento <- 11
	x@rating <- "BB"
	class(x) <- "AyrtonPosition"
}



createAyrtonOpzioni_su_azioni1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo53'
	x@Strumento <- 'OA'
	x@Moneta <- 'CHF'
	x@Saldo <- -100
	x@Nome <- "-100 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90 / 10"
	x@NumeroValore <- ""
	x@ValoreMercatoMonetaCHF <- -5840
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 18
	class(x) <- "AyrtonPosition"
}



createAyrtonOpzioni_su_azioni2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo186'
	x@Strumento <- 'OA'
	x@Moneta <- 'CHF'
	x@Saldo <- -500
	x@NumeroValore <- ""
	x@Nome <- "-500 / PUT / Credit Suisse Group Na / 21-12-12 / Strike 46 / Premio(112267 CHF) / CH0012138530 / 17.71 / 10"
	x@ValoreMercatoMonetaCHF <- -107100
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 18	
	class(x) <- "AyrtonPosition"
	
}



createAyrtonOpzioni_su_azioni3 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo53'
	x@Strumento <- 'OA'
	x@Moneta <- 'CHF'
	x@Saldo <- 100
	x@NumeroValore <- ""
	x@Nome <- "100 / PUT / Syngenta AG / 17-02-12 / Strike 290 / Premio(-5500 CHF) / CH0011027469 / 337.90 / 10"
	x@ValoreMercatoMonetaCHF <- 5840
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 18
	class(x) <- "AyrtonPosition"
	
}



createAyrtonOpzioni_su_azioni4 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo186'
	x@Strumento <- 'OA'
	x@Moneta <- 'CHF'
	x@Saldo <- 500
	x@NumeroValore <- ""
	x@Nome <- '500 / Call / Credit Suisse Group Na / 21-12-12 / Strike 46 / Premio(-112267 CHF) / CH0012138530 / 17.71 / 10'
	x@ValoreMercatoMonetaCHF <- 107100
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 18
	class(x) <- "AyrtonPosition"
}



createAyrtonOpzioni_su_divise1 <- function() {
	# create a valid opzioni_su_divise
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "OV"
	x@Moneta <- "USD"
	x@Saldo <- 125000
	x@NumeroValore <- ""
	x@Nome <- "PUT 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"
	x@ValoreMercatoMonetaCHF <- 7751.37
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 19
	class(x) <- "AyrtonPosition"
}



createAyrtonOpzioni_su_divise2 <- function() {
	
	# create a valid opzioni_su_divise
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "OV"
	x@Moneta <- "USD"
	x@Saldo <- -250000
	x@NumeroValore <- ""
	x@Nome <- "Call 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
	x@ValoreMercatoMonetaCHF <- -44.03
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 19
	class(x) <- "AyrtonPosition"
}



createAyrtonOpzioni_su_divise3 <- function() {
	# create a valid opzioni_su_divise
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "OV"
	x@Moneta <- "USD"
	x@Saldo <- 125000
	x@NumeroValore <- ""
	x@Nome <- "Call 17-08-12 Strike 1.295 EUR 125000 Premio(-8293.75 USD)"
	x@ValoreMercatoMonetaCHF <- 7751.37
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 19
	class(x) <- "AyrtonPosition"
}



createAyrtonOpzioni_su_divise4 <- function() {
	# create a valid opzioni_su_divise
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "OV"
	x@Moneta <- "USD"
	x@Saldo <- -250000
	x@NumeroValore <- ""
	x@Nome <- "PUT 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
	x@ValoreMercatoMonetaCHF <- -44.03
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 19
	class(x) <- "AyrtonPosition"
}



createAyrtonOpzioni_su_obbligazioni <- function() {
	# create a valid opzioni_su_obbligazioni
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "OV"
	x@Moneta <- "EUR"
	x@Saldo <- 125000
	x@NumeroValore <- ""
	x@Nome <- "PUT 17-08-12 Strike 103.5 EUR 125000 Premio(-345.45 EUR) EU0011027469 "
	x@ValoreMercatoMonetaCHF <- 242.47
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 20
	class(x) <- "AyrtonPosition"
}



createAyrtonProRata1 <- function() {
	# create a valid AccruedInterest
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- "Oacc"
	x@Moneta <- "CHF"
	x@Saldo <- 50000
	x@NumeroValore <- "2403071"
	x@Nome <- "20120221 - 2% Toyota 21-02-12 Pro-rata"
	x@ValoreMercatoMonetaCHF <- 796.818833446452
	x@ID_AAA <- 1073
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
}



createAyrtonProRataFondiObbligazionari <- function() {
	# create a valid AccruedInterest di Fondi_obbligazionari
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "Oacc"
	x@Moneta <- "EUR"
	x@Saldo <- 10000
	x@NumeroValore <- "2490099"
	x@Nome <- "20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20 Pro-rata"
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- 825
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
}



createAyrtonProRataObbligazioni_convertibili <- function() {
	# create a valid AccruedInterest di Obbligazioni_convertibili
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "Oacc"
	x@Moneta <- "CHF"
	x@Saldo <- 25000
	x@NumeroValore <- "CH0190462702"
	x@Nome <- "20130329 - 4% CS 29-03-13 Pro-rata"
	x@ValoreMercatoMonetaCHF <- 431.78
	x@ID_AAA <- 2209
	x@ID_strumento <- 11
	class(x) <- "AyrtonPosition"
}



createAyrtonStrutturati_EQ1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo53'
	x@Strumento <- 'PS'
	x@Moneta <- 'USD'
	x@Saldo <- 5e+05
	x@NumeroValore <- "FR0010429068"
	x@Nome <- 'Certificat EMI Emerging Market Momentum'
	x@ValoreMercatoMonetaCHF <- 350385.56
	x@ID_AAA <- 119
	x@ID_strumento <- 48
	class(x) <- "AyrtonPosition"
}



createAyrtonStrutturati_EQ2 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo53'
	x@Strumento <- 'PS'
	x@Moneta <- 'EUR'
	x@Saldo <- 4176
	x@NumeroValore <- "CH0124311439"
	x@Nome <- '20121228 - Bull Certificate EXANE SX5E Dividend Dec12'
	x@ValoreMercatoMonetaCHF <- 580316.156620992
	x@ID_AAA <- 121
	x@ID_strumento <- 48
	class(x) <- "AyrtonPosition"
}



createAyrtonStrutturati_FI <- function() {
	# create a Strutturati_FI security
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "PS"
	x@Moneta <- "EUR"
	x@Saldo <- 150000
	x@NumeroValore <- "11439214"
	x@Nome <- "20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS"
	x@ValoreMercatoMonetaCHF <- 179299.42998
	x@ID_AAA <- 98
	x@ID_strumento <- 49
	class(x) <- "AyrtonPosition"
}



createAyrtonStrutturati_FI1 <- function() {
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo3'
	x@Strumento <- 'PS'
	x@Moneta <- 'EUR'
	x@Saldo <- 150000
	x@NumeroValore <- "11439214"
	x@Nome <- '20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS'
	x@ValoreMercatoMonetaCHF <- 179299.42998
	x@ID_AAA <- 98
	x@ID_strumento <- 49
	class(x) <- "AyrtonPosition"
}



createAyrtonUnclassified <- function() {
	# create an Unclassified security
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- ""
	x@Moneta <- "CHF"
	x@Saldo <- 100.30
	x@NumeroValore <- "abc"
	x@Nome <- "Security not classified"
	x@ValoreMercatoMonetaCHF <- 123.55
	x@ID_AAA <- 1073
	x@ID_strumento <- 53
	class(x) <- "AyrtonPosition"
}
