
# import the required libraries and set the homeDir
# and sourceCodeDir variables
if (.Platform$OS.type=="windows") {
	library("utils")
	library("tcltk",quietly=TRUE,verbose=FALSE)
	library("rJava",quietly=TRUE)
	library("Rbbg",quietly=TRUE)
	homeDir <- "C:/riskman"
	# use the "Progra~1" syntax for R CMD BATCH use
	sourceCodeDir <- "C:/Progra~1/R/riskman"
 } else {
 	sourceCodeDir <- getwd()
 }


logger <- function(x) {}

# set the last action before exiting the application
.Last <- function() {
	cat("Process halted\n")
}

# print a welcome message
cat("\nWelcome to riskman!\n\n")

# start the application
if (.Platform$OS.type=="windows") {
	source("c:/Program Files/R/riskman/adviceManagement.R")
}

# print a termination message
cat("Program terminated.\n\n")


