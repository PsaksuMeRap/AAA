# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./odbc/connessioni.R")

clienteFI <- "pippo76" # Euro fixed income
clienteGec <- "pippo210" # Global economy
clienteGeq <- "pippo53" # Global equity

fetchDate <- "2011-12-30"
desiredFields <- c("Cliente",
		"Strumento",
		"Moneta",
		"Nome",
		"Categoria",
		"ValoreMercatoMonetaCHF",
		"ID_AAA","ID_strumento",
		"Scadenza")

datiFI  <- importDBPortfolioGeneraleDataFrame(c=clienteFI,f=fetchDate)[,desiredFields]
datiGec <- importDBPortfolioGeneraleDataFrame(c=clienteGec,f=fetchDate)[,desiredFields]
datiGeq <- importDBPortfolioGeneraleDataFrame(c=clienteGeq,f=fetchDate)[,desiredFields]

# prendi repositoryAssetClassMapping
repo <- create_repositoryAssetClassMapping()
areMissing <- logical(nrow(datiFI))

for (i in 1:nrow(datiFI)) {
	areMissing[i] <- !any(is.element(datiFI[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

fieldsNames <- c("ID_strumento","ID_AAA","Strumento","Moneta","Nome")
if (any(areMissing)) {
	missing <- datiFI[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/missingInstrForAssetClassMapping_FI.csv")
}


# ora Global Economy
areMissing <- logical(nrow(datiGec))

for (i in 1:nrow(datiGec)) {
	areMissing[i] <- !any(is.element(datiGec[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

if (any(areMissing)) {
	missing <- datiGec[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/missingInstrForAssetClassMapping_Gec.csv")
}



# ora Global Equity
areMissing <- logical(nrow(datiGeq))

for (i in 1:nrow(datiGeq)) {
	areMissing[i] <- !any(is.element(datiGeq[i,"Nome"],repo$assetClassMapping.df[["Nome"]]))
}

if (any(areMissing)) {
	missing <- datiGeq[areMissing,fieldsNames]
	write.csv(missing,file="./repositories/missingInstrForAssetClassMapping_Geq.csv")
}




