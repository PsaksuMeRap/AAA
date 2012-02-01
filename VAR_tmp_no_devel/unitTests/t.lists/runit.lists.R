# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.shouldTestIsListFieldEqual <- function() {
	source("./lib/lists.R")
	
	origin <- list(
			list(a=1,b="cane"),
			list(a=2,b="gatto"),
			list(a=1,b="gatto"),
			list(a=4,b="pippo"),		
			list(a=2,b="claudio")
	)
	
	# filtra by a=1
	should <- c(TRUE,FALSE,TRUE,FALSE,FALSE)
	filtered <- isListFieldEqual(origin,"a",1)
	checkEquals(filtered,should)
	
	# filtra by b="cane" (ritorna una lista con 1 solo elemento)
	should <- c(TRUE,FALSE,FALSE,FALSE,FALSE)
	filtered <- isListFieldEqual(origin,"b","cane")
	checkEquals(filtered,should)	
	
	# filtra by b="lld" 
	should <- c(FALSE,FALSE,FALSE,FALSE,FALSE)
	filtered <- isListFieldEqual(origin,"b","lld")
	checkEquals(filtered,should)

	# filtra by b="lld" (ritorna lista vuota!)
	origin <- list()
	should <- list()
	filtered <- isListFieldEqual(origin,"b","lld")
	checkEquals(filtered,should)
	
} 



test.shouldFilterEmptylist <- function() {
	source("./lib/lists.R")
	
	origin <- list()
	filtered <- filterLists(origin,by="a",value=1)
	checkEquals(filtered,list())
}


test.shouldFilterListByCriterium <- function() {
	source("./lib/lists.R")
		
	origin <- list(
			list(a=1,b="cane"),
			list(a=2,b="gatto"),
			list(a=1,b="gatto"),
			list(a=4,b="pippo"),		
			list(a=2,b="claudio")
	)
	
	# filtra by a=1
	should <- list(
			list(a=1,b="cane"),
			list(a=1,b="gatto")
	)
	filtered <- filterLists(origin,"a",1)
	checkEquals(filtered,should)

	# filtra by b="cane" (ritorna una lista con 1 solo elemento)
	should <- list(
			list(a=1,b="cane")
	)
	filtered <- filterLists(origin,"b","cane")
	checkEquals(filtered,should)	
	
	# filtra by b="lld" (ritorna lista vuota!)
	should <- list()
	filtered <- filterLists(origin,"b","lld")
	checkEquals(filtered,should)
	
} 


test.shouldFilterListByMultipleCriterium <- function() {
	source("./lib/lists.R")
	
	origin <- list(
			list(a=1,b="cane"),
			list(a=2,b="gatto"),
			list(a=1,b="gatto"),
			list(a=4,b="pippo"),		
			list(a=2,b="claudio")
	)
	
	# filtra by a=c(1,2)
	should <- list(
			list(a=1,b="cane"),
			list(a=2,b="gatto"),
			list(a=1,b="gatto"),	
			list(a=2,b="claudio")
	)
	filtered <- filterLists(origin,"a",c(1,2))
	checkEquals(filtered,should)
	
	# filtra by b=c("claudio","gatto")
	should <- list(
			list(a=2,b="gatto"),
			list(a=1,b="gatto"),		
			list(a=2,b="claudio")
	)
	filtered <- filterLists(origin,"b",c("claudio","gatto"))
	checkEquals(filtered,should)
} 


test.shouldExtractFieldName <- function() {
	source("./lib/lists.R")
	
	origin <- list(
			list(a=1,b="cane"),
			list(a=2,b="gatto"),
			list(a=1,b="gatto"),
			list(a=4,b="pippo"),		
			list(a=2,b="claudio")
	)
	
	# extract a
	extracted <- extractFromList(origin,"a")
	checkEquals(extracted,c(1,2,1,4,2))
	
	# extract b (ritorna una lista con 1 solo elemento)
	origin <- list(
			list(a=1,b="cane")
	)
	extracted <- extractFromList(origin,"b")
	checkEquals(extracted,"cane")	
	
	# filtra by b="lld" (ritorna lista vuota!)
	origin <- list()
	extracted <- extractFromList(origin,"b")
	checkEquals(extracted,origin)
	
} 
