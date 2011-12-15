# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/repository.R")
source("./lib/money.R")
source("./lib/position/extendPosition.R")

setClass("Position",representation(name="character",money="Money",origin="list"),prototype(name=NA_character_,money=new("Money"),origin=list()))


create_position <- function() {
	position <- list()
	class(position) <- "position"
	
	position$name = NA_character_
	position$money = toMoney()
	position$origin = NA
	
	position$create <- function(name=NA_character_,currency="CHF",
			amount=0.0) {
		position$name <<- name
		position$money <<- toMoney(amount,currency)
	}
	
	return(position)
}

