# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldExplodeFondo_misto <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# crea position di tipo accrued interest e Fondo_misto
	nomi <- c("Cliente","Strumento","Moneta","Saldo","NumeroValore",
			"Nome","PrezzoMedio","PrezzoMercato","ValorePosizione",
			"ValoreMonetaRiferimento","Evoluzione","Ordine","Categoria",
			"ValoreMercatoMonetaCHF","ValoreMercatoMonetaEUR",
			"ValoreMercatoMonetaUSD","ID_AAA","ID_strumento",
			"ID_transazione","Ticker_indice_per_BETA","Valuta",
			"VariazioneFX")
	
	origin1 <- list("pippo16","A","EUR",4477.2698043909,"",
			"25.3-74.7 Fondo misto di test",NA,NA,4477.2698043909,4477.2698043909,
			NA,"A","Fondo misto",5975.0597818207,4477.2698043909,
			6250.71637391013,12,26,NA,NA,NA,NA)
	names(origin1) <- nomi
	
	
	origin2 <- list("pippo16","Oacc","EUR",0,"2490099",
			"20201231 - 0% CB-Accent Lux Sicav - Fixed Income EUR 31-12-20 Pro-rata",
			NA,NA,0,0,NA,"D","Obbligazioni e simili          ",0,0,0,825,
			2,NA,NA,NA,NA)
	names(origin2) <- nomi
	
	# initialize the different repositories
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	allocateTestRepositories("exchangeRates")
	
	# create client portfolio
	# create position 1
	parser <- create_parserPosition()
	position1 <- parser$parse(origin1)
	
	# create position 2
	parser <- create_parserPosition()
	position2 <- parser$parse(origin2)
	
	# Test 1: explode un fondo misto (risultato di tipo positions)
	result <- explode(position1)
	
	checkEquals(class(result),"positions")
	checkEquals(class(result$positions[[1]]),c("Fondi_azionari","position"))
	checkEquals(class(result$positions[[2]]),c("Fondi_obbligazionari","position"))	
	total <- result$positions[[1]]$money$sum(result$positions[[2]]$money)
	checkEquals(total,position1$money$amount)
	
	# Test 2: explode una posizione di tipo non fondo misto (risultato: la position stessa)
	result <- explode(position2)
	checkEquals(result,position2)
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
}
