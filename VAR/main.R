# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
home <- "/home/claudio/workspace/AAA/VAR/"
setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()

source("./lib/library.R")
