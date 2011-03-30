# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_finTimeseries <- function() {
	
	name = "nome"
	data = data.frame(date=c("2010-01-02","2010-01-03"),value=c(100,100.10))
	
	finTsLevel <- create_finTsLevel(name=name,data=data)
	
	checkEquals(class(finTsLevel),c("finTsLevel","finTimeseries"))
	checkEquals(finTsLevel$name, "nome")
	checkEquals(finTsLevel$data,data)
	
}

test.finTsLevel_verifyPositivity <- function() {
	
	# Test 1
	name = "nome"
	date=c("2010-01-02","2010-01-03","2011-02-23")
	value <- c(100,100.10,0)
	data = data.frame(date=date,value=value)
	
	finTsLevel <- create_finTsLevel(name=name,data=data)
	checkEquals(finTsLevel$verifyPositivity(),1)
	
	# Test 2
	name = "nome"
	date=c("2010-01-02","2010-01-03")
	value <- c(100,100.10)
	data = data.frame(date=date,value=value)
	
	finTsLevel <- create_finTsLevel(name=name,data=data)
	checkEquals(finTsLevel$verifyPositivity(),1)
	
	# Test 3
	name = "nome"
	date=c("2010-01-02","2010-01-03","2010-01-04","2011-02-23")
	value <- c(100,100.10,101,150.35)
	data = data.frame(date=date,value=value)
	
	finTsLevel <- create_finTsLevel(name=name,data=data)
	checkEquals(finTsLevel$verifyPositivity(),0)
	
}