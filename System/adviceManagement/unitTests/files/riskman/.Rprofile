
options(papersize="a4")
options(help_type="html")

options(defaultPackages = c("utils"))

if (.Platform$OS.type=="windows") {
	library("tcltk",quietly=TRUE,verbose=FALSE)
	library("stringr",quietly=TRUE)
	library("rJava",quietly=TRUE)
	library("Rbbg",quietly=TRUE)
	homeDir <- "C:/riskman"
	sourceCodeDir <- "C:/Users/Claudio/workspace/AAA/System"
 } else {
 	library("tcltk",quietly=TRUE,verbose=FALSE)
	library("stringr",quietly=TRUE)
	library("rJava",quietly=TRUE)
	homeDir <- "/home/claudio/riskman"
	sourceCodeDir <- "/home/claudio/workspace/AAA/System"
 }

logger <- function(x) {}

.Last <- function() {
	logger("Process halted\n")
	sink()
}
cat("\nWelcome to riskman!\n\n") 


