# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# create the data.frame of the instruments
file <- file.path(systemOptions[["sourceCodeDir"]],"data","instruments","repositoryInstruments.csv")
instruments.df <- read.csv(file=file,header=TRUE,stringsAsFactors=FALSE)
colnames(instruments.df) <- c("ID","Instrument")
rm(file)

# create the instrument repository
instruments <- create_repositoryInstruments(instruments.df)
rm(instruments.df)

# assign the repository	
assign("instruments",instruments,envir=repositories)