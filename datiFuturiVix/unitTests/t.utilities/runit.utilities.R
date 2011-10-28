# TODO: Add comment
# 
# Author: ortellic
###############################################################################


#test.determineBusinessDate <- function(Date,nbDays) {
test.determineBusinessDate <- function() {
	# questa funzione calcola la data lavorativa che segue o precede Date
	# di nbDays giorni
	
	Date = "2011-10-03"

	# test 1
	nbDays = 0
	result <- determineBusinessDate(Date,nbDays)
	checkEquals(result,"2011-10-03")

	# test 2
	nbDays = 5
	result <- determineBusinessDate(Date,nbDays)
	checkEquals(result,"2011-10-10")

	# test 3
	nbDays = 13
	result <- determineBusinessDate(Date,nbDays)
	checkEquals(result,"2011-10-20")

	# test 4
	nbDays = -1
	result <- determineBusinessDate(Date,nbDays)
	checkEquals(result,"2011-09-30")
	
	# test 4
	nbDays = -1
	result <- determineBusinessDate(Date,nbDays)
	checkEquals(result,"2011-09-30")
	
	# test 5
	nbDays = -10
	result <- determineBusinessDate(Date,nbDays)
	checkEquals(result,"2011-09-19")
	
}
