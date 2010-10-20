source("C:/Documents and Settings/claudio.AYRTON/Desktop/R_tablelist/tcltk_utilities.R")
# Viene impostata la connessione al database
odbcConnection = setOdbcConnection("DBMarkowitz")
dati <- get.table(odbcConnection, "Copia_DBAzioni")
parent <- tktoplevel()

b <- create_tablelist(parent,dati,withScrollBarX=TRUE,withScrollBarY=TRUE)
tkgrid(b$frame)


