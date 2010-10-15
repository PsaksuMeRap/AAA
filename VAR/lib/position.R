# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")

create_positions <- function() {
	positions <- new.env()
	class(positions) <- "positions"
	
	positions$positions <- list()
	
	positions$addPosition <- function(pos) {
		# pos: a position
		positions$positions[[length(positions$positions)+1]] <<- pos
	}
	
	positions$addPositions <- function(pos) {
		# pos: an onbject of class positions
		lapply(pos$positions,positions$addPosition)
	}
	
	positions$remove <- function(index) {
		if (missing(index)) index <- length(positions$positions)
		positions$positions[[index]] <<- NULL
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
	
	position$print <- function() {
		print(position$name)
		print(position$currency)
		print(position$amount)
	}
	
	position$isMemberOf <- function(myClass) {
		classes <- class(position)
		if (any(classes == myClass)) return(TRUE) else return(FALSE)
	}
	
	position$extendEquities <- function() {
		# create the repository of the equities if not available
		
		if (!exists("equities",envir=repositories)) {
			eval(expression(equities <- create_repositoryEquities())
					,env=repositories)
		}
		
		# identify the ticker of the equity
		id <- position$origin[["ID_AAA"]]
		if (is.na(id)) {print("Errore: azione senza ID_AAA"); stop()}
		id <- as.numeric(id)
		
		position$ticker <<- repositories$equities$tickerFromId(id)		
		return()		
		
	}
	
	position$class <- function() return(class((position)))
	
	return(position)
}

