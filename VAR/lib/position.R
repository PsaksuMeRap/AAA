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
	
	positions$sum <- function(toCurrency) {
		# toCurrency: the currency to convert to
		total <- toMoney(0,toCurrency)
		for (p in positions$positions) {
			total$sum(toMoney(p$amount,p$currency))
		}
		return(total)
	}
	
	positions$remove <- function(index) {
		if (missing(index)) index <- length(positions$positions)
		positions$positions[[index]] <<- NULL
	}
	
	positions$print <- function() {
		#df <- positions$toDataFrame()
		#print(df)
		criteria <- c("instrument","currency","name")
		for (p in positions$sortBy()) p$print()
	}
	
	positions$sortBy <- function(criteria) {
		# creteria: an ordered character vector with the following criteria
		# class - currency - name - amount

		df <- positions$toDataFrame()
		indices <- do.call(order,df[criteria])
		return(positions$positions[indices])
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


create_position <- function() {
	position <- new.env() # list()
	class(position) <- "position"
	
	position$name = NA_character_
	position$currency = NA_character_
	position$amount = NA_real_
	position$origin = NA
	
	position$create <- function(name=NA_character_,currency="CHF",
			amount=0.0,origin=NA) {
		position$name <<- name
		position$currency <<- currency
		position$amount <<- amount
		position$origin <<- origin
	}
	
	
	position$isMemberOf <- function(myClass) {
		classes <- class(position)
		if (any(classes == myClass)) return(TRUE) else return(FALSE)
	}
	
	position$extendEquities <- function() {
		# create the repository of the equities if not available
		
		if (!exists("equities",envir=repositories,inherits=FALSE)) {
			eval(expression(equities <- create_repositoryEquities())
					,env=repositories)
		}
		
		# identify the ticker of the equity
		id <- position$origin[["ID_AAA"]]
		if (is.na(id)) {print("Errore: azione senza ID_AAA"); stop()}
		id <- as.numeric(id)
		
		position$ticker <<- repositories$equities$tickerFromId(id)
		position$print <<- function()
		{
			print(paste(class(position)[1],"/",position$currency, "-", position$amount,
							"/ Name:", position$name,
							"/ Ticker:", position$ticker
						)
					)
		}
		return()		
		
	}
	
	position$print <- function() {
		print(paste(class(position)[1],"/",position$currency,"-", position$amount, "/ Name:", position$name))
	}
	
	#elimina questa funzione se non serve
	# position$class <- function() return(class((position)))
	
	position$toDataFrame <- function() {
		# this function create a data.frame from the list of positions
		df <- data.frame(instrument=class(position)[1],
				name=position$name,
				currency=position$currency,
				amount=position$amount,stringsAsFactors=FALSE
				)
		return(df)
	}
	return(position)
}

