# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldReturnIdFromId <- function() {
	# crea la classe Id come classe virtuale contenente "numeric" e "character"
	setClass("IdCharacter",contains="character")
	setClassUnion("Id",c("numeric","IdCharacter"))
	
	# test an id of class IdCharacter
	idCharacter <- new("IdCharacter","pippo")
	checkEquals(idForGroupBy(idCharacter),"pippo")
	
	# test an id of class numeric
	idNumeric <- new("numeric",1234)
	checkEquals(idForGroupBy(idNumeric),"1234")
	
	# test an id of class IdAyrton with IdAAA_numeric
	IdAyrton1 <- new("IdAyrton",idAAA=new("IdAAA_numeric",4321),idStrumento=1234)
	checkEquals(idForGroupBy(IdAyrton1),"4321__1234")
	
	# test an id of class IdAyrton with IdAAA_character
	IdAyrton2 <- new("IdAyrton",idAAA=new("IdAAA_character","pippo23mamma"),idStrumento=1234)
	checkEquals(idForGroupBy(IdAyrton2),"pippo23mamma__1234")
}
