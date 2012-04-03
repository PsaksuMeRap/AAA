# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RUnit")
home <- "/home/claudio/workspace/AAA/datastream/"
setwd(home)

stringsAsFactors = FALSE

source("./lib/library.R")

importer <- create_importer(importFrom="./unitTests/data/serie storiche datastream.csv")
repository <- importer$createRepository()
rm(importer)

plot.dsTimeseries <- function(x) {
	plot(as.Date(rownames(x$data)),x$data[,1],xlab=x$name,type="l",ylab="")
}

plot(repository[[1]])