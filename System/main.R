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

source("./base/lib/library.R")
systemOptions[["sourceCodeDir"]] <- getwd()

## -- fine setup 

