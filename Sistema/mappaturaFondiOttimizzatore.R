# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./odbc/connessioni.R")

# i dati per la mappatura tra posizione e benchmark sono nel
# file repositoryAssetClassMapping.csv nella directory ./repositories/assetClassMapping/
# Eventuali posizioni non mappate sono indicate nei rispettivi files csv nella medesima directory

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
	result <- cbind(loopOver.df,Pesi=result[as.matrix(loopOver.df)])
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
	write.csv(missing,file="./repositories/assetClassMapping/missingInstrForAssetClassMapping_FI.csv")
} else {
	resultFixedIncome <- aggregatePositions(datiFixedIncome)
	write.csv(resultFixedIncome,file="./repositories/assetClassMapping/pf_fixedIncome.csv")	
}

# ora Global Economy
areMissing <- logical(nrow(datiGlobalEconomy))

for (i in 1:nrow(datiGlobalEconomy)) {
	areMissing[i] <- !any(is.element(datiGlobalEconomy[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

if (any(areMissing)) {
	missing <- datiGlobalEconomy[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/assetClassMapping/missingInstrForAssetClassMapping_Gec.csv")
} else {
	resultGlobalEconomy <- aggregatePositions(datiGlobalEconomy)
	write.csv(resultGlobalEconomy,file="./repositories/assetClassMapping/pf_GlobalEconomy.csv")
}

# ora Global Equity
areMissing <- logical(nrow(datiGlobalEquity))

for (i in 1:nrow(datiGlobalEquity)) {
	areMissing[i] <- !any(is.element(datiGlobalEquity[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

if (any(areMissing)) {
	missing <- datiGlobalEquity[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/assetClassMapping/missingInstrForAssetClassMapping_Geq.csv")
} else {
	resultGlobalEquity <- aggregatePositions(datiGlobalEquity)
	write.csv(resultGlobalEquity,file="./repositories/assetClassMapping/pf_GlobalEquity.csv")
}


# importa le serie dei prezzi
names <- read.csv(file="./repositories/datiPerCovarianze/dati chf.csv",as.is=TRUE,header=FALSE,nrow=1)
dati.df <- read.csv(file="./repositories/datiPerCovarianze/dati chf.csv",as.is=TRUE)
colnames(dati.df) <- names; rm(names)
rowNames <- as.character(as.Date(paste(dati.df[[1]],dati.df[[2]],dati.df[[3]],sep="-")))
rownames(dati.df) <- rowNames; rm(rowNames)
dati.df <- dati.df[,-(1:4)]

# rimuovi le serie dei tassi d'interesse
rimuovi <- c("US INTERBANK 1 WEEK","EUR INTERBANK 1 WEEK","CH INTERBANK 1 WEEK")
daTenere <- !is.element(colnames(dati.df),rimuovi)

# calcola i rendimenti logaritmici
logReturns <- diff(as.matrix(log(dati.df[,daTenere])))

# calcola le covarianze
Sigma <- cov(logReturns,use="pairwise.complete.obs")



estendiConRischioCambio <- function(monetaRiferimento,x) {

	monete <- unique(x[["Moneta"]])
	monete <- setdiff(monete,monetaRiferimento)
	if (length(monete)>0) {
		for (moneta in monete) {
			stessaMoneta <- is.element(x[["Moneta"]],moneta)
			peso <- sum(x[stessaMoneta,"Pesi"])
			new <- data.frame("benchmark"=moneta,"Moneta"=moneta,"Pesi"=peso)
			if (peso!=0) {
				if (exists("extend")) extend <- rbind(extend,new) else extend <- new
			}
		}
		if (exists("extend")) {
			if (nrow(extend)>0) x <- rbind(x[,-1],extend)
		}
	}
	
	# elimina posizioni con skip
	elimina <- is.element(x[["benchmark"]],"skip")
	x <- x[!elimina,,drop=FALSE]
	return(x)
}

QfDataFrameGeq <- estendiConRischioCambio("CHF",resultGlobalEquity)
QfDataFrameGec <- estendiConRischioCambio("CHF",resultGlobalEconomy)
QfDataFrameFi  <- estendiConRischioCambio("EUR",resultFixedIncome)

Qf <- function(QfData,Sigma) {

	nomi <- QfData[["benchmark"]]
	nomiSenzaCovarianze <- setdiff(nomi,colnames(Sigma))
	if (length(nomiSenzaCovarianze)>0) {
		print(nomiSenzaCovarianze)
		stop("Ci sono nomi senza covarianze!")
	}
	subSigma <- Sigma[nomi,nomi]
	pesi <- as.vector(QfData[,"Pesi"])
	return(pesi%*%subSigma%*%pesi)
}


volFixedIncome <- sqrt(Qf(QfDataFrameFi,Sigma)*252)
volGlobalEquity  <- sqrt(Qf(QfDataFrameGeq,Sigma)*252)
volGlobalEconomy <- sqrt(Qf(QfDataFrameGec,Sigma)*252)

