# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./odbc/connessioni.R")

fondoFixedIncome <- "pippo76" # Euro fixed income
fondoGlobalEconomy <- "pippo210" # Global economy
fondoGlobalEquity <- "pippo53" # Global equity

fetchDate <- "2012-01-27"
desiredFields <- c("Cliente",
		"Strumento",
		"Moneta",
		"Nome",
		"Categoria",
		"ValoreMercatoMonetaCHF",
		"ID_AAA","ID_strumento",
		"Scadenza")

datiFixedIncome  <- importDBPortfolioGeneraleDataFrame(c=fondoFixedIncome,f=fetchDate)[,desiredFields]
datiGlobalEconomy <- importDBPortfolioGeneraleDataFrame(c=fondoGlobalEconomy,f=fetchDate)[,desiredFields]
datiGlobalEquity <- importDBPortfolioGeneraleDataFrame(c=fondoGlobalEquity,f=fetchDate)[,desiredFields]

# prendi repositoryAssetClassMapping
repo <- create_repositoryAssetClassMapping()

# verifica che non ci siano posizioni che si ripetano nel repositoryAssetClassMapping
lNames <- length(repo$assetClassMapping.df[["Nome"]])
lUniqueNames <- length(unique(repo$assetClassMapping.df[["Nome"]]))
if (!identical(lNames,lUniqueNames)) stop("Verifica doppioni nel file repositoryAssetClassMapping.csv")
rownames(repo$assetClassMapping.df) <- repo$assetClassMapping.df[["Nome"]]


newColumns <- function(desiredNames) {
	# questa funzione restituisce le colonne assetClass e benchmark da aggiungere 
	# alle colonne esistenti.
	result <- repo$assetClassMapping.df[desiredNames,c("assetClass","benchmark")]
	rownames(result) <- NULL
	return(result)
}

aggregatePositions <- function(x) {
	# x: il data.frame contenente le posizioni del portafoglio
	# questa funziona calcola e raggruppa i pesi % rispetto ai benchmark 
browser()	
	# seleziona i nomi delle posizioni in portafoglio
	currentPortfolioNames <- x[["Nome"]]
	# determina le assetClass ed i benchmark corrispondenti
	nuoveColonne.df <- newColumns(currentPortfolioNames)
	# calcola il totale del portafoglio in CHF
	total <- sum(x[,"ValoreMercatoMonetaCHF"])
	# calcola i valori percentuali
	x[["ValoreMercatoMonetaCHF"]] <- x[["ValoreMercatoMonetaCHF"]] / total
	# aggiungi le nuove colonne ai dati
	nuoveColonne.df <- cbind(nuoveColonne.df,x["Moneta"])
	# determina le combinazioni per le quali aggregare
	loopOver.df <- unique(nuoveColonne.df)
	# applica il filtro
	result <- tapply(x[["ValoreMercatoMonetaCHF"]],nuoveColonne.df,FUN=sum,simplify=TRUE)
	# aggiungi il risultato al data.frame con i fattori <assetClass,benchmark,moneta>
	result <- cbind(loopOver.df,Pesi=result[as.matrix(loopOver)])
	rownames(result) <- NULL
	return(result)
}

# ora Fixed Income
areMissing <- logical(nrow(datiFixedIncome))

for (i in 1:nrow(datiFixedIncome)) {
	areMissing[i] <- !any(is.element(datiFixedIncome[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

fieldsNames <- c("ID_strumento","ID_AAA","Strumento","Moneta","Nome")
if (any(areMissing)) {
	missing <- datiFixedIncome[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/missingInstrForAssetClassMapping_FI.csv")
} else {
	resultFixedIncome <- aggregatePositions(datiFixedIncome)
}

# ora Global Economy
areMissing <- logical(nrow(datiGlobalEconomy))

for (i in 1:nrow(datiGlobalEconomy)) {
	areMissing[i] <- !any(is.element(datiGlobalEconomy[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

if (any(areMissing)) {
	missing <- datiGlobalEconomy[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/missingInstrForAssetClassMapping_Gec.csv")
} else {
	resultGlobalEconomy <- aggregatePositions(datiGlobalEconomy)
}

# ora Global Equity
areMissing <- logical(nrow(datiGlobalEquity))

for (i in 1:nrow(datiGlobalEquity)) {
	areMissing[i] <- !any(is.element(datiGlobalEquity[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

if (any(areMissing)) {
	missing <- datiGlobalEquity[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/missingInstrForAssetClassMapping_Geq.csv")
} else {
	resultGlobalEquity <- aggregatePositions(datiGlobalEquity)
}






