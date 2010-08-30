# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))
library("RODBC")
source ("/home/claudio/Dropbox/eclipse/calcoloCovarianze/sub_db_utilities.R")
source ("procedureImportazioneDati.R")

options(browser="google-chrome")
options(help_type="html")

source("connessioni.R")

workDir <- getwd()


## ----------------------------------------------------------------------------
## inizia l'imporazione delle azioni
## ----------------------------------------------------------------------------
# importa la lista dei nomi dei files da importare

directory <- "Dati/azioni"
listaFiles <- list.files(paste(workDir,"/",directory,sep=""))

if (length(listaFiles) == 0) {
	print ("non ci sono azioni da importare")
} else {
	x = lapply(listaFiles,importaDaCvs,workDir,directory)
	success <- lapply(x,importaAzioniInBancaDati)
	print ("Terminata importazione azionaria")
}


## ----------------------------------------------------------------------------
## inizia l'imporazione delle azioni_aggiunte
## ----------------------------------------------------------------------------
# importa la lista dei nomi dei files da importare

directory <- "Dati/azioni_aggiunte"
listaFiles <- list.files(paste(workDir,"/",directory,sep=""))

if (length(listaFiles) == 0) {
	print ("non ci sono azioni da importare")
} else {
	x = lapply(listaFiles,importaDaCvs,workDir,directory)
	success <- lapply(x,importaAzioniInBancaDati)
	print ("Terminata importazione azioni aggiunte")
}


## ----------------------------------------------------------------------------
## inizia l'imporazione dei cambi
## ----------------------------------------------------------------------------
# importa la lista dei nomi dei files da importare
directory <- "/Dati/cambi"
listaFiles <- list.files(paste(workDir,"/",directory,sep=""))

if (length(listaFiles) == 0) {
	print ("non ci sono cambi da importare")
} else {
	
	x = lapply(listaFiles,importaDaCvs,workDir,directory)
	success <- lapply(x,importaCambiInBancaDati)
	print ("Terminata importazione cambi")
}


## ----------------------------------------------------------------------------
## inizia l'imporazione dei fondi nostri
## ----------------------------------------------------------------------------
# importa la lista dei nomi dei files da importare
directory <- "/Dati/fondi_senza_proxy"
listaFiles <- list.files(paste(workDir,"/",directory,sep=""))

if (length(listaFiles) == 0) {
	print ("non ci sono fondi nostri da importare")
} else {
	
	x = lapply(listaFiles,importaDaCvs,workDir,directory)
	success <- lapply(x,importaFondiSenzaProxyInBancaDati)
	print ("Terminata importazione fondi nostri")
}


## ----------------------------------------------------------------------------
## inizia l'imporazione degli indici sulle azioni
## ----------------------------------------------------------------------------
# importa la lista dei nomi dei files da importare

directory <- "Dati/indici_azioni_e_di_vola"
listaFiles <- list.files(paste(workDir,"/",directory,sep=""))

if (length(listaFiles) == 0) {
	print ("non ci sono indici azionari da importare")
} else {
	x = lapply(listaFiles,importaDaCvs,workDir,directory)
	success <- lapply(x,importaIndiciAzioniInBancaDati)
	print ("Terminata importazione indici azioni")
}


## ----------------------------------------------------------------------------
## inizia l'imporazione dei metalli preziosi
## ----------------------------------------------------------------------------
# importa la lista dei nomi dei files da importare
directory <- "Dati/metalli_preziosi"
listaFiles <- list.files(paste(workDir,"/",directory,sep=""))

if (length(listaFiles) == 0) {
	print ("non ci sono metalli preziosi da importare")
} else {
	x = lapply(listaFiles,importaDaCvs,workDir,directory)
	success <- lapply(x,importaMetalliPreziosiInBancaDati)
	print ("Terminata importazione metalli preziosi")
}


## ----------------------------------------------------------------------------
## inizia l'imporazione dei tassi d'interesse
## ----------------------------------------------------------------------------
# importa la lista dei nomi dei files da importare
directory <- "Dati/tassi_di_interesse"
listaFiles <- list.files(paste(workDir,"/",directory,sep=""))

if (length(listaFiles) == 0) {
	print ("non ci sono tassi d'interesse da importare")
} else {
	x = lapply(listaFiles,importaDaCvs,workDir,directory)
	success <- lapply(x,importaTassiInteresseInBancaDati)
	print ("Terminata importazione tassi di interesse")
}




