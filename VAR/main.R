# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RODBC")
source("connessioni.R")

stringsAsFactors = FALSE
repositories <- new.env()



