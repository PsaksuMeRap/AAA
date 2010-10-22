# TODO: Add comment
# 
# Author: claudio
###############################################################################



bonds.df <- read.csv("./dati/fi.csv",header=TRUE)
colnames(bonds.df) <- c("rating","scadenza","nome","settore","moneta",
		"peso","valore")

# extract gov bonds
isGovBond <- bonds.df[,"Settore"] == "Government"
govBonds.df <- bonds.df[isGovBond,]
nonGovBonds.df <- bonds.df[!isGovBond,]

tapply(govBonds.df[,"valore"],govBonds.df[,c("moneta","rating")],sum)