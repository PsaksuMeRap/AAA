# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RUnit")
library("XML")
source("./library.R")

testo <- readLines(con="./dati/Huyhn.htm",encoding="ISO-8859-1")
testo <- iconv(testo,from="ISO-8859-1",to="UTF-8")

nomeCognome <- identifyStudent(testo)

tables <- extractTablesBlocks(testo)

tabellaPrimoSemestre <- filterTableByDesiredString(tables,"1° semestre")[[1]]
tabellaSecondoSemestre <- filterTableByDesiredString(tables,"2° semestre")[[1]]
tabellaTerzoSemestre <- filterTableByDesiredString(tables,"3° semestre")[[1]]
tabellaCorsiAggiuntivi <- filterTableByDesiredString(tables,"Corsi aggiuntivi")[[1]]
tabellaCorsiEsteri <- filterTableByDesiredString(tables,"Corsi esteri")[[1]]
tabellaFuoriSede <- filterTableByDesiredString(tables,"Semestre fuori sede")[[1]]
tabella <- filterTableByDesiredString(tables,"Tesi")[[1]]


primoSemestre <- parseTabellaXSemestre(tabellaPrimoSemestre)
secondoSemestre <- parseTabellaXSemestre(tabellaSecondoSemestre)
terzoSemestre <- parseTabellaXSemestre(tabellaTerzoSemestre)
corsiAggiuntivi <- parseTabellaXSemestre(tabellaCorsiAggiuntivi)
corsiEsteri <- parseTabellaXSemestre(tabellaCorsiEsteri)

