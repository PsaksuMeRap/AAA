# TODO: Add comment
# 
# Author: Claudio
###############################################################################


#library("tcltk")
#library("stringr")
#library("RODBC")

#if(.Platform$OS.type=="windows") {
#	library("rJava")
#	library("Rbbg")
#}

stringsAsFactors = FALSE

repositories <- new.env()

if (.Platform$OS.type=="windows") {
	if (!exists("homeDir",inherits=FALSE)) homeDir <- "C:/riskman"
} else {
	if (!exists("homeDir",inherits=FALSE)) homeDir <- "/home/claudio/riskman"
}

cat("Starting daemon ...\n\n")
# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
if (!exists("sourceCodeDir",inherits=FALSE)) sourceCodeDir <- getwd()

source(file.path(sourceCodeDir,"adviceManagement","mainProcess.R"))

