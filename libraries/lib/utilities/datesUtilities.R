# TODO: Add comment
# 
# Author: claudio
###############################################################################


completeDatePart <- function(v.dayOrMonth)
{
	## this function complete with a zero the daily or monthly part
	## of a date, i.e 3 -> 03.
	## v.dayOrMonth: a vector containing the day [1-31] or the month [1-12].
	
	toComplete <- nchar(v.dayOrMonth,allowNA = TRUE) == 1
	v.dayOrMonth[toComplete] = paste("0",v.dayOrMonth[toComplete],sep="")
	return(v.dayOrMonth)
}

