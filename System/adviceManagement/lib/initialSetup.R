# TODO: Add comment
# 
# Author: claudio
###############################################################################


library("RODBC")
library("RUnit")
library("tcltk")
library("stringr")

stringsAsFactors = FALSE

source("./base/lib/library.R")

# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
systemOptions[["sourceCodeDir"]] <- sourceCodeDir
rm(sourceCodeDir)

# set the homeDir, i.e. the directory where the archive and the postBox directories must be installed
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","homeDir.R"))
systemOptions[["homeDir"]] <- homeDir
rm(homeDir)
