# TODO: Add comment
# 
# Author: Claudio
###############################################################################

#rm(list=ls(all=TRUE))

#library("RODBC")
#library("RUnit")
#library("tcltk")
#library("stringr")

#stringsAsFactors = FALSE
#repositories <- new.env()

#if (.Platform$OS.type=="windows") {
#	homeDir <- "C:/riskman"
#} else {
#	homeDir <- "/home/claudio/riskman"
#}

#source("./base/lib/library.R")
#sys[["sourceCodeDir"]] <- getwd()

## -- fine setup 
library("tcltk")
library("stringr")
library("RODBC")

if(.Platform$OS.type=="windows") {
	library("rJava")
	library("Rbbg")
}

stringsAsFactors = FALSE

repositories <- new.env()

if (.Platform$OS.type=="windows") {
	homeDir <- "C:/riskman"
} else {
	homeDir <- "/home/claudio/riskman"
}

# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
sourceCodeDir <- getwd()

source(file.path(sourceCodeDir,"adviceManagement","mainProcess.R"))

