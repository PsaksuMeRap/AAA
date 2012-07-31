# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

stringsAsFactors = FALSE
repositories <- new.env()

if (.Platform$OS.type=="windows") {
	homeDir <- "C:/riskman"
} else {
	homeDir <- "/home/claudio/riskman"
}

source("./base/lib/library.R")
sys[["sourceCodeDir"]] <- getwd()

## -- fine setup 



