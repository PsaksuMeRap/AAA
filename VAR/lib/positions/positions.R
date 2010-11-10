# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")
source("./lib/money.R")

create_positions <- function() {
	positions <- new.env()
	class(positions) <- "positions"
	
	positions$positions <- list()
	
	positions$add <- function(p) {
		# p: a position or an object of class positions
		if (is.element("position",class(p))) {
			positions$addPosition(p)
			return()
		}
		
		if (is.element("positions",class(p))) {
			positions$addPositions(p)
			return()
		}
		stop("Impossible to add the position: p is not of class position or positions")
	}
	
	positions$addPosition <- function(pos) {
		# pos: a position
		positions$positions[[length(positions$positions)+1]] <<- pos
	}
	
	positions$addPositions <- function(pos) {
		# pos: an object of class positions
		lapply(pos$positions,positions$addPosition)
	}
	
	positions$extract <- function(isToExtract) {
		# isToExtract: a vector of TRUE/FALSE indicating
		# which component must be extracted
		
		positions_new <- create_positions()
		
		if (missing(isToExtract) | identical(isToExtract,NULL)) return(positions_new)
		if (length(isToExtract)!=length(positions$positions)) {
			stop("positions$extract: lenght of list != TRUE/FALSE vector")
		}
		
		positions_new$positions <- positions$positions[isToExtract]
		return(positions_new)
	}
	
	positions$isCurrency <- function(currency) {
		result <- lapply(positions$positions,
				function(x,currency) return(x$isCurrency(currency)),
				currency)
		return(result)
	}
	
	positions$print <- function() {
		criteria <- c("instrument","currency","name")
		for (p in positions$sortBy()) p$print()
	}
	
	positions$toString <- function() {
		x <- sapply(positions$sortBy(),
				function(p) return(p$toString())
		)
		return(x)
	}
	
	positions$remove <- function(index) {
		# if remove is called without argument, the last element is
		# removed
		if (missing(index)) index <- length(positions$positions)
		
		if (mode(index)=="logical") {
			# determine the indices to remove
			index <- (1:length(index))[index]
		}
		positions$positions[index] <<- NULL
		
	}
	
	positions$sortBy <- function(criteria) {
		# creteria: an ordered character vector with the following criteria
		# class - currency - name - amount
		
		df <- positions$toDataFrame()
		indices <- do.call(order,df[criteria])
		return(positions$positions[indices])
	}
	
	positions$sum <- function(toCurrency) {

		if (missing(toCurrency)) {
			if (length(positions$positions)>0) {
				toCurrency <- positions$positions[[1]]$currency
			} else {
				return(toMoney(0,"CHF"))
			}
		}
		# toCurrency: the currency to convert to
		total <- toMoney(0,toCurrency)
		for (p in positions$positions) {
			total$sum(toMoney(p$amount,p$currency))
		}
		return(total)
	}	
	
	positions$toDataFrame <- function() {
		# this function create a data.frame from the list of positions

        toDataFrame <- function(x){return(x$toDataFrame())}
		result <- lapply(positions$positions,toDataFrame)
		
		# create an empty data.frame
		df <- data.frame(instrument=character(0),name=character(0),currency=character(0),amount=numeric(0))
		
		if (length(result)==0) return(df)
		
		for (res in result) df <- rbind(df,res)
		
		# remove the rownames from df
		rownames(df) <- NULL
		
		return(df)
	}
	
	return(positions)
}



