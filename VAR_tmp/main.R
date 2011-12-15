# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RODBC")
library("RUnit")
home <- "/home/claudio/workspace/AAA/VAR_tmp/"
home <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/VAR_tmp"
setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()

source("./lib/library.R")
