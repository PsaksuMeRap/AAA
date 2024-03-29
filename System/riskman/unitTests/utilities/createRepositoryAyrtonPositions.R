# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryAyrtonPositions <- function() {
	
	testData <- new.env()

	# create a valid equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo160"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 15
	x@Nome <- "Roche Holding Gs"
	x@ValoreMercatoMonetaCHF <- 88205
	x@ID_AAA <- 824
	x@ID_strumento <- 1
	
	class(x) <- "AyrtonPosition"

	testData$equity1 <- x
	
	
	# create a valid equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "A"
	x@Moneta <- "EUR"
	x@Saldo <- 1000
	x@Nome <- "Kontron AG"
	x@ValoreMercatoMonetaCHF <- 7439.7503136
	x@ID_AAA <- 1772
	x@ID_strumento <- 1
	
	class(x) <- "AyrtonPosition"
	
	testData$equity2 <- x
	
	
	# create a valid indexCertificate
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo210"
	x@Strumento <- "A"
	x@Moneta <- "USD"
	x@Saldo <- 100
	x@Nome <- "ISHARES MSCI Indon"
	x@ValoreMercatoMonetaCHF <- 283354.88
	x@ID_AAA <- 283354.88
	x@ID_strumento <- 15
	
	class(x) <- "AyrtonPosition"
	
	testData$indexCertificate <- x
	
	
	# create a non existing equity
	#x <- new("AyrtonPosition")
	#x@Cliente <- "pippo160"
	#x@Strumento <- "A"
	#x@Moneta <- "CHF"
	#x@Saldo <- 19
	#x@Nome <- "Non existing"
	#x@ValoreMercatoMonetaCHF <- 88205
	#x@ID_AAA <- 100020202
	#x@ID_strumento <- 1
	#class(x) <- "AyrtonPosition"
	
	# testData$noExists <- x
	
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 100000
	x@Nome <- "20130603 - 3.625% Pfizer 03-06-13"
	x@ValoreMercatoMonetaCHF <- 124345.632268
	x@ID_AAA <- 1218
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond1 <- x
	
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 300000
	x@Nome <- "20120410 - 1.503% EIB FRN 10-04-12"
	x@ValoreMercatoMonetaCHF <- 362217.41556
	x@ID_AAA <- 1976
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond2 <- x
	
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 200000
	x@Nome <- "20120319 - 1.869% Rabobank Nederland 19-03-12"
	x@ValoreMercatoMonetaCHF <- 241164.668888
	x@ID_AAA <- 1967
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond3 <- x
	
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- "O      "
	x@Moneta <- "CHF"
	x@Saldo <- 50000
	x@Nome <- "20120221 - 2% Toyota 21-02-12"
	x@ValoreMercatoMonetaCHF <- 50025
	x@ID_AAA <- 1073
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond4 <- x
	
	
	# create a valid  Fondi_obbligazionari
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 10000
	x@Nome <- "20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	x@ValoreMercatoMonetaCHF <- 10300
	x@ID_AAA <- 825
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
	
	testData$fondiObbligazionari <- x
	
	
	# create a valid AccruedInterest di Fondi_obbligazionari
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "Oacc"
	x@Moneta <- "EUR"
	x@Saldo <- 10000
	x@Nome <- "20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20 Pro-rata"
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- 825
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
		
	testData$proRataFondiObbligazionari <- x
	
	
	# create a valid AccruedInterest
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- "Oacc"
	x@Moneta <- "CHF"
	x@Saldo <- 50000
	x@Nome <- "20120221 - 2% Toyota 21-02-12 Pro-rata"
	x@ValoreMercatoMonetaCHF <- 796.818833446452
	x@ID_AAA <- 1073
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$proRata1 <- x
	
	# create an Unclassified security
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- ""
	x@Moneta <- "CHF"
	x@Saldo <- 100.30
	x@Nome <- "Security not classified"
	x@ValoreMercatoMonetaCHF <- 123.55
	x@ID_AAA <- 1073
	x@ID_strumento <- 53
	class(x) <- "AyrtonPosition"
	
	testData$unclassified1 <- x
	
	
	# create a Strutturati_FI security
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "PS"
	x@Moneta <- "EUR"
	x@Saldo <- 150000
	x@Nome <- "20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS"
	x@ValoreMercatoMonetaCHF <- 179299.42998
	x@ID_AAA <- 98
	x@ID_strumento <- 49
	class(x) <- "AyrtonPosition"

	testData$strutturati_FI <- x
	
	
	# create a Fondi_misti
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo84"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 400
	x@Nome <- "70-30 UBS Strategy Fund Yield CHF"
	x@ValoreMercatoMonetaCHF <- 46988
	x@ID_AAA <- 1476
	x@ID_strumento <- 26
	class(x) <- "AyrtonPosition"
	
	testData$Fondi_misti <- x	
	
	
	# create a globalEquity position
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo123"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 2000
	x@Nome <- "OnCapital Global Equity Fund Cap B"
	x@ValoreMercatoMonetaCHF <- 147480
	x@ID_AAA <- 1701
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"
	
	testData$globalEquity <- x	
	
	
	# create a fixedIncome position
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo16"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 11000
	x@Nome <- "CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	x@ValoreMercatoMonetaCHF <- 1686274.5866043
	x@ID_AAA <- 825
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
	
	testData$fixedIncome <- x	
	##---------------------
	
	# create a globalEconomy position
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo51"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 100
	x@Nome <- "CB-Accent Global Economy"
	x@ValoreMercatoMonetaCHF <- 9263
	x@ID_AAA <- 2256
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"
	
	testData$globalEconomy <- x

	
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo88'
	x@Strumento <- 'Oacc'
	x@Moneta <- 'EUR'
	x@Saldo <- 48
	x@Nome <- '20201231 - 0% UBS MM EUR 31-12-20 Pro-rata'
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- 469
	x@ID_strumento <- 4
	class(x) <- "AyrtonPosition"
	testData$Fondi_mercato_monetario1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo165'
	x@Strumento <- 'O      '
	x@Moneta <- 'USD'
	x@Saldo <- 9
	x@Nome <- '20201231 - 0% Aberdeen Liquidity Fund 31-12-20'
	x@ValoreMercatoMonetaCHF <- 25246.17324864
	x@ID_AAA <- 162
	x@ID_strumento <- 4
	class(x) <- "AyrtonPosition"
	testData$Fondi_mercato_monetario2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo75'
	x@Strumento <- 'Oacc'
	x@Moneta <- 'EUR'
	x@Saldo <- 65000
	x@Nome <- '20130307 - 0% Commezbk (variabile da 07-03-2007 a 07-03-2013) 07-03-13 Pro-rata'
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- 1060
	x@ID_strumento <- 5
	class(x) <- "AyrtonPosition"
	testData$Floating_rate_notes1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo75'
	x@Strumento <- 'O      '
	x@Moneta <- 'EUR'
	x@Saldo <- 65000
	x@Nome <- '20130307 - 0% Commezbk (variabile da 07-03-2007 a 07-03-2013) 07-03-13'
	x@ValoreMercatoMonetaCHF <- 77853.223734
	x@ID_AAA <- 1060
	x@ID_strumento <- 5
	class(x) <- "AyrtonPosition"
	testData$Floating_rate_notes2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo47'
	x@Strumento <- 'Macc'
	x@Moneta <- 'CHF'
	x@Saldo <- 0
	x@Nome <- 'Anticipo fisso 01-04-09/02-04-12 Ipoteca tasso fisso 115.000 CHF 2.05% Pro-rata'
	x@ValoreMercatoMonetaCHF <- 5.84250046813395e-05
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 6
	class(x) <- "AyrtonPosition"
	testData$Anticipi_fissi1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo131'
	x@Strumento <- 'M'
	x@Moneta <- 'CHF'
	x@Saldo <- -1e+05
	x@Nome <- 'Anticipo fisso 20-12-11/14-02-12 Novers 0.65%'
	x@ValoreMercatoMonetaCHF <- -1e+05
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 6
	class(x) <- "AyrtonPosition"
	testData$Anticipi_fissi2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo11'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 157
	x@Nome <- 'Pictet SICAV Water P Cap'
	x@ValoreMercatoMonetaCHF <- 30388.376629908
	x@ID_AAA <- 1840
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"
	testData$Fondi_azionari1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo14'
	x@Strumento <- 'A'
	x@Moneta <- 'CHF'
	x@Saldo <- 250
	x@Nome <- 'OnCapital Global Equity Fund Cap B'
	x@ValoreMercatoMonetaCHF <- 17115
	x@ID_AAA <- 1701
	x@ID_strumento <- 14
	class(x) <- "AyrtonPosition"
	testData$Fondi_azionari2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo210'
	x@Strumento <- 'A'
	x@Moneta <- 'USD'
	x@Saldo <- 10000
	x@Nome <- 'ISHARES MSCI Indon'
	x@ValoreMercatoMonetaCHF <- 283354.88
	x@ID_AAA <- 2272
	x@ID_strumento <- 15
	class(x) <- "AyrtonPosition"
	testData$Index_certificate1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo210'
	x@Strumento <- 'A'
	x@Moneta <- 'USD'
	x@Saldo <- 10000
	x@Nome <- 'SPDR KBW ETF'
	x@ValoreMercatoMonetaCHF <- 201963.76
	x@ID_AAA <- 2273
	x@ID_strumento <- 15
	class(x) <- "AyrtonPosition"
	testData$Index_certificate2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo53'
	x@Strumento <- 'OA'
	x@Moneta <- 'CHF'
	x@Saldo <- -1000
	x@Nome <- '-1000 Call Syngenta AG 17-02-12 Strike 290 Premio(5500 CHF)'
	x@ValoreMercatoMonetaCHF <- -5840
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 18
	class(x) <- "AyrtonPosition"
	testData$Opzioni_su_azioni1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo186'
	x@Strumento <- 'OA'
	x@Moneta <- 'CHF'
	x@Saldo <- -5000
	x@Nome <- '-5000 PUT Credit Suisse Group Na 21-12-12 Strike 46 Premio(112267 CHF)'
	x@ValoreMercatoMonetaCHF <- -107100
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 18
	class(x) <- "AyrtonPosition"
	testData$Opzioni_su_azioni2 <- x
	##---------------------
	
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo210'
	x@Strumento <- 'F'
	x@Moneta <- 'CHF'
	x@Saldo <- -25
	x@Nome <- 'Swiss Index Futures (SMI) Futures 16-03-2012                    '
	x@PrezzoMercato <- 6445
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 50
	class(x) <- "AyrtonPosition"
	testData$Futures_EQ1 <- x
	##---------------------
	
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo15'
	x@Strumento <- 'XAU'
	x@Moneta <- 'USD'
	x@Saldo <- 180
	x@Nome <- 'XAU'	
	x@ValoreMercatoMonetaCHF <- 284105.4768
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 21
	class(x) <- "AyrtonPosition"
	testData$Metalli_preziosi1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo16'
	x@Strumento <- 'FX'
	x@Moneta <- 'CHF'
	x@Saldo <- -829060.081320845
	x@Nome <- 'CHF -1,000,000.00 Valuta 26-03-2012'
	x@ValoreMercatoMonetaCHF <- -1e+06
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 22
	class(x) <- "AyrtonPosition"
	testData$FX_Forward1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo71'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 2298.4
	x@Nome <- 'UBS WM Global Property Fund EUR'
	x@ValoreMercatoMonetaCHF <- 18225.6287818451
	x@ID_AAA <- 1884
	x@ID_strumento <- 25
	class(x) <- "AyrtonPosition"
	testData$Fondi_immobiliari1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo186'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 29946
	x@Nome <- 'Rights on Banco Santander'
	x@ValoreMercatoMonetaCHF <- 4623.4140158976
	x@ID_AAA <- 2422
	x@ID_strumento <- 30
	class(x) <- "AyrtonPosition"
	testData$Diritti_aumento_capitale_azionario1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo11'
	x@Strumento <- 'L'
	x@Moneta <- 'CHF'
	x@Saldo <- 219365.039954224
	x@Nome <- 'CHF-16.4105.2120.001.01'
	x@ValoreMercatoMonetaCHF <- 219365.039954224
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 40
	class(x) <- "AyrtonPosition"
	testData$Conto_corrente1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo11'
	x@Strumento <- 'L'
	x@Moneta <- 'EUR'
	x@Saldo <- 32678.3407778931
	x@Nome <- 'EUR-16.4105.2120.814.01'
	x@ValoreMercatoMonetaCHF <- 39416.1310068511
	x@ID_AAA <- NA_real_
	x@ID_strumento <- 40
	class(x) <- "AyrtonPosition"
	testData$Conto_corrente2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo72'
	x@Strumento <- 'A'
	x@Moneta <- 'CHF'
	x@Saldo <- 0.29
	x@Nome <- 'GEMS PROGRESSIVE FD SICAV LOW VOLATILITY RESERVE POOL'
	x@ValoreMercatoMonetaCHF <- 291.9517
	x@ID_AAA <- 1869
	x@ID_strumento <- 44
	class(x) <- "AyrtonPosition"
	testData$Fondi_Hedge1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo192'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 1844.31
	x@Nome <- 'Signet Credit I EUR Class Segregated Portfolio'
	x@ValoreMercatoMonetaCHF <- 259474.944273368
	x@ID_AAA <- 1953
	x@ID_strumento <- 44
	class(x) <- "AyrtonPosition"
	testData$Fondi_Hedge2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo9'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 5100
	x@Nome <- 'Lyxor ETF CRB'
	x@ValoreMercatoMonetaCHF <- 134472.7632072
	x@ID_AAA <- 2277
	x@ID_strumento <- 45
	class(x) <- "AyrtonPosition"
	testData$ETF_equity1 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo9'
	x@Strumento <- 'A'
	x@Moneta <- 'EUR'
	x@Saldo <- 45000
	x@Nome <- 'Lyxor ETF MSCI Em Mkts'
	x@ValoreMercatoMonetaCHF <- 428690.281932
	x@ID_AAA <- 2278
	x@ID_strumento <- 45
	class(x) <- "AyrtonPosition"
	testData$ETF_equity2 <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo53'
	x@Strumento <- 'PS'
	x@Moneta <- 'USD'
	x@Saldo <- 5e+05
	x@Nome <- 'Certificat EMI Emerging Market Momentum'
	x@ValoreMercatoMonetaCHF <- 350385.56
	x@ID_AAA <- 119
	x@ID_strumento <- 48
	class(x) <- "AyrtonPosition"
	testData$Strutturati_EQ <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo53'
	x@Strumento <- 'PS'
	x@Moneta <- 'EUR'
	x@Saldo <- 4176
	x@Nome <- '20121228 - Bull Certificate EXANE SX5E Dividend Dec12'
	x@ValoreMercatoMonetaCHF <- 580316.156620992
	x@ID_AAA <- 121
	x@ID_strumento <- 48
	class(x) <- "AyrtonPosition"
	testData$Strutturati_EQ <- x
	##---------------------
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo3'
	x@Strumento <- 'PS'
	x@Moneta <- 'EUR'
	x@Saldo <- 150000
	x@Nome <- '20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS'
	x@ValoreMercatoMonetaCHF <- 179299.42998
	x@ID_AAA <- 98
	x@ID_strumento <- 49
	class(x) <- "AyrtonPosition"
	testData$Strutturati_FI1 <- x
	##---------------------
	
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo202'
	x@Strumento <- 'A'
	x@Moneta <- 'USD'
	x@Saldo <- 3000
	x@Nome <- 'Gold Bullion Securities'
	x@ValoreMercatoMonetaCHF <- 434745.41
	x@ID_AAA <- 2430
	x@ID_strumento <- 51
	class(x) <- "AyrtonPosition"
	testData$ETF_commodities_gold <- x
	##---------------------
	
	
	x <- new("AyrtonPosition")
	x@Cliente <- 'pippo202'
	x@Strumento <- 'PS'
	x@Moneta <- 'EUR'
	x@Saldo <- 500000
	x@Nome <- '20130404 - Credit-linked Note UBS Jersey (Credit of Air France)'
	x@ValoreMercatoMonetaCHF <- 600743.26
	x@ID_AAA <- 169
	x@ID_strumento <- 52
	class(x) <- "AyrtonPosition"
	testData$Credit_linked_note <- x
	##---------------------
	
	
	return(testData)
}

create_ayrtonPosition <- function(i,df) {
	string <- "x <- new('AyrtonPosition')\n"
	string <- paste(string,"x@Cliente <- '",df[i,"Cliente"],"'\n",sep="")
	string <- paste(string,"x@Strumento <- '",df[i,"Strumento"],"'\n",sep="")
	string <- paste(string,"x@Moneta <- '",df[i,"Moneta"],"'\n",sep="")
	string <- paste(string,"x@Saldo <- ",df[i,"Saldo"],"\n",sep="")	
	string <- paste(string,"x@Nome <- '",df[i,"Nome"],"'\n",sep="")
	string <- paste(string,"x@PrezzoMercato <- '",df[i,"PrezzoMercato"],"'\n",sep="")
	string <- paste(string,"x@ValoreMercatoMonetaCHF <- ",df[i,"ValoreMercatoMonetaCHF"],"\n",sep="")	
	string <- paste(string,"x@ID_AAA <- ",df[i,"ID_AAA"],"\n",sep="")	
	string <- paste(string,"x@ID_strumento <- ",df[i,"ID_strumento"],"\n",sep="")	
	string <- paste(string,"x@Saldo <- ",df[i,"Saldo"],"\n",sep="")	
	string <- paste(string,"class(x) <- 'AyrtonPosition'\n",sep="")	
	string <- paste(string,"testData$xxx <- x\n",sep="")
	string <- paste(string,"##---------------------\n\n",sep="")
	return(string)
}

#id <- c(645,980,560,571,318,824,3,217,373,374,479,1055,232,236,514,634,1058,6,7,536,895,166,167,463,464,22,46,364,465)
#a <- sapply(id,create_ayrtonPosition,dati.df)
rm(create_ayrtonPosition)
