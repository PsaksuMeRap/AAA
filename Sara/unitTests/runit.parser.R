# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.shouldIdentifyTableStartLine <- function(){

	lineFalse <- "ciao claudio questo non start table"
	lineTrue <- "<table width=1024 border=0 cellpadding=0 cellspacing=0>"

	check1 <- isStartTable(lineFalse)
	check2 <- isStartTablel(lineTrue)
	checkEquals(check1,FALSE)
	checkEquals(check2,TRUE)
}

test.shouldIdentifyTableEndLine <- function(){
	
	lineFalse <- "ciao claudio questo non start table"
	lineTrue <- "         </table>    "
	
	check1 <- isEndTable(lineFalse)
	check2 <- isEndTable(lineTrue)
	checkEquals(check1,FALSE)
	checkEquals(check2,TRUE)
}


test.shouldReturnStartEndTableLine <- function() {
	testo <- "test"
	testo[2] <- "<table width=1024 border=0 cellpadding=0 cellspacing=0>"
	testo[3] <- "jlsadjf j table"
	testo[4] <- "    </tr></table>"
	testo[5] <- "aiuto che funziona?"
	testo[6] <- ""
	testo[7] <- "<table cellpadding=\"2\" cellspacing=\"2\" with=\"100%\" class=\"testo_normale\">"
	testo[8] <- ""
	testo[9] <- "  jdjd  </table> kfkf"
	testo[10] <- "no"	
	
	# testo <- readLines(con="./dati/Huyhn.htm",encoding="Latin-1")
	
	startEndLineNumbers <- returnStartEndTableLineNumbers(testo)
	checkEquals(length(startEndLineNumbers),2)
	checkEquals(startEndLineNumbers[[2]],list(inizio=7,fine=9))	
}


test.extractTablesBlocks <- function() {
	testo <- "test"
	testo[2] <- "<table width=1024 border=0 cellpadding=0 cellspacing=0>"
	testo[3] <- "jlsadjf j table"
	testo[4] <- "    </tr></table>"
	testo[5] <- "aiuto che funziona?"
	testo[6] <- ""
	testo[7] <- "<table cellpadding=\"2\" cellspacing=\"2\" with=\"100%\" class=\"testo_normale\">"
	testo[8] <- ""
	testo[9] <- "  jdjd  </table> kfkf"
	testo[10] <- "no"	
	
	testo1 <- "<table width=1024 border=0 cellpadding=0 cellspacing=0>"
	testo1[2] <- "jlsadjf j table"
	testo1[3] <- "    </tr></table>"
	
	testo2 <- "<table cellpadding=\"2\" cellspacing=\"2\" with=\"100%\" class=\"testo_normale\">"
	testo2[2] <- ""
	testo2[3] <- "  jdjd  </table> kfkf"
	
	blocchi <- extractTablesBlocks(testo)
	
	checkEquals(blocchi[[1]],testo1)
	checkEquals(blocchi[[2]],testo2)
}


check.identifyStudent <- function() {
	testo <- readLines(con="./dati/Huyhn.htm",encoding="Latin-1")
	nomeCognome <- identifyStudent(testo)
	
	checkEquals(nomeCognome$nome,"Michele")
	checkEquals(nomeCognome$cognome,"Huynh")
}


check.identifyTable <- function() {
	
	table1 <- "akdlkjd"
	table1[2] <- "kdkdkd"
	table1[3] <- "Totale provvisorio crediti ECTS"
	table1[4] <- "aiuto"

	table2 <- "akdlkjd"
	table2[2] <- "kdkdkd"
	table2[3] <- "Totale provvisorio credit ECTS"
	table2[4] <- "aiuto"
	table2[5] <- "aiutooooo"

	tables <- list(table1,table2)
	
	stringToIdentify <- "Totale provvisorio crediti ECTS"
	checkEquals(isInTable(table1,stringToIdentify),TRUE)
	checkEquals(isInTable(table2,stringToIdentify),FALSE)
	
	result <- filterTableByDesiredString(tables,stringToIdentify)
	checkEquals(result,list(table1))
}



