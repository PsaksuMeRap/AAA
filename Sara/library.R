# TODO: Add comment
# 
# Author: claudio
###############################################################################


isStartTable <- function(line) {
	grepl("<table",line,useBytes=TRUE)
}


isEndTable <- function(line) {
	grepl("</table",line,useBytes=TRUE)
}


returnStartEndTableLineNumbers <- function(testo) {
	isStartLine <- unlist(lapply(testo,isStartTable))
	isEndLine <- unlist(lapply(testo,isEndTable))
	nbLines <- length(testo)
	startLines <- (1:nbLines)[isStartLine]
	endLines <- (1:nbLines)[isEndLine]
	
	# verifica che il numero di linee d'inizio sia uguale a quelle di fine
	if (length(endLines)!=length(startLines)) {
		stop("numero di righe endLine diverso da startLine")
	}
	
	nrLinea <- 1
	index <- list()
	for (line in startLines) {
		index[[nrLinea]] <- list(inizio=line,fine=endLines[nrLinea])
		nrLinea <- nrLinea+1
	}
	return(index)
}


extractTablesBlocks <- function(testo) {
	startEndLineNumbers <- returnStartEndTableLineNumbers(testo)
	
	extractBlock <- function(x,testo) {
		fromTo <- x$inizio:x$fine
		return(testo[fromTo])
	}
	
	blocchi <- lapply(startEndLineNumbers,extractBlock,testo)
	return(blocchi)
}


removeStartEndSpaces <- function(string) {
	result <- sub("^[[:blank:]]+", "", string)
	result <- sub("[[:blank:]]+$", "", result)
	return(result)
}


isInTable <- function(table,string) {
	tmp <- function(line,string) return(grepl(string,line))
	any(unlist(lapply(table,tmp,string)))
}


filterTableByDesiredString <- function(tables,string) {
	isTheTable <- unlist(lapply(tables,isInTable,string))
	if (any(isTheTable)) table <- tables[isTheTable] else table <- list()
}


identifyStudent <- function(testo) {
	stop=FALSE
	for (line in testo) {
		if (stop) break
		if (grepl("Scheda riassunto esami",line,useBytes=TRUE)) stop=TRUE
	}
	res1 <- strsplit(line,"F\">")
	res1 <- res1[[1]][2]
	res2 <- strsplit(res1,"&nbsp;")
	cognome <- res2[[1]][1]
	nome <- res2[[1]][2]
	# rimuovi lo spazio finale
	cognome <- removeStartEndSpaces(cognome)
	nome <- substr(nome,1,nchar(nome)-4)
	return(list(nome=nome,cognome=cognome))
}

printTableToFile <- function(table) {
	table <- iconv(table,to="ISO-8859-1",from="UTF-8")
	cat("<html>",file="test.html")
	cat("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">",file="test.html",append=TRUE)
	cat(table,file="test.html",append=TRUE)
	cat("</html>",file="test.html",append=TRUE)
}

htmlParseTable <- function(table) {
	printTableToFile(table)
	result <- readHTMLTable("test.html",trim=TRUE,header=FALSE,stringsAsFactors=FALSE)
	file.remove("./test.html")
	return(result)
}

parseTabellaXSemestre <- function(tabella) {
	dataFrame <- htmlParseTable(tabella)[[1]]
	dataFrame <- dataFrame[-(1:2),]
	colNames <- as.character(dataFrame[1,])
	dataFrame <- dataFrame[-1,]
	
	newDf <- dataFrame[[1]]
	
	for (i in 2:ncol(dataFrame)) {
		newDf <- data.frame(newDf,as.numeric(dataFrame[,i]),
				stringsAsFactors = FALSE)
	}
	
	colnames(newDf) <- colNames
	return(newDf)
}

parseTabellaFuoriSede <- function(tabella) {
	dataFrame <- htmlParseTable(tabella)[[1]]
	dataFrame <- dataFrame[-1,]
	
	fuoriSedeTmp <- dataFrame[2:4,]
	stageTmp <- dataFrame[7:8,]
	tesiTmp <- dataFrame[12:17,]
	
	
	fuoriSede <- list()
	fuoriSede$svolto <- fuoriSedeTmp[1,2]
	fuoriSede$esonero <- fuoriSedeTmp[2,2]
	fuoriSede$dettaglio <- fuoriSedeTmp[3,2]
	
	
	stage <- list()
	stage$svolto <- stageTmp[1,2]
	stage$ects=as.numeric(stageTmp[2,2])
	
	
	tesi <- list()
	tesi$Titolo <- tesiTmp[1,2]
	tesi$Relatore <- tesiTmp[2,2]
	tesi$Correlatore <- tesiTmp[3,2]	
	tesi$ECTS <- tesiTmp[4,2]
	tesi$Data_difesa <- tesiTmp[5,2]	
	tesi$Nota <- tesiTmp[6,2]
	
	return(newDf)
}
