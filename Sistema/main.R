# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")

if(.Platform$OS.type=="windows") {
	home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/Sistema/base/"
} else {
	home <- "/home/claudio/workspace/AAA/Sistema/base"
}

setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()

source("./lib/library.R")

## -- fine setup

