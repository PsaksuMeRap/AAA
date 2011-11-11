# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RUnit")

stringsAsFactors = FALSE

source("./lib/library.R")
source("./unitTests/testUtilities.R")



# calcolo di u_t

create_symbolWithRandom <- function(symbolName,randomName,index) {
	create_monomials(
			create_monomial(1,
					create_symbols(create_symbol(paste(symbolName,index,sep=""),1)),
					create_randomVariables(create_randomVariable(randomName,index,1))
			)
	)
}


u_t <- create_symbolWithRandom(symbolName="b",randomName="eps",index=0)

for (i in 1:5) u_t <- u_t + create_symbolWithRandom(symbolName="b",randomName="eps",index=i)

u_tPower4 <- sort(u_t*u_t*u_t*u_t)
toString(u_tPower4)



	