# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreateMessage <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files")
	
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
	advisors[["MaggiDynamic"]] <- new("Advisor",name="MaggiDynamic",folderName="MaggiDynamic",email="maggi.sandro@")
	
	result <- messageFactory(fileName,directory,advisors)
	
	checkEquals(result[["fileExtension"]],"csv")
	checkEquals(result[["date"]],"2012-05-09")
	checkEquals(result[["time"]],"14-22-24")	
	checkEquals(result[["from"]],"GhidossiGlobalEquity")
	checkEquals(result[["messageType"]],"newAdvice")
	checkEquals(result[["fileName"]],"2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv")
	checkEquals(result@advisor,advisors[["GhidossiGlobalEquity"]])
	checkEquals(is(result,"NewAdvice"),TRUE)
	checkEquals(result@trades[[1]]@securityID,"RocheGA")
	
	
	
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_confirmation.csv"
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(is(result,"Confirmation"),TRUE)
	checkEquals(result@trades[[2]]@securityID,"SMI")
	
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_preComplianceResult_1.csv"
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(is(result,"PreComplianceResult"),TRUE)
	
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_postComplianceResult_0.csv"
	result <- messageFactory(fileName,directory,advisors)
	checkEquals(is(result,"PostComplianceResult"),TRUE)
	
}


test.shouldTestGetMessageDate_time_from <- function() {
	# identify a new order
	fileName <- "2012-05-09_14-22-24_GhidossiGlobalEquity_newAdvice.csv"
	
	# define the adivisors
	advisors <- new("Advisors")
	advisors[["GhidossiGlobalEquity"]] <- new("Advisor",name="GhidossiGlobalEquity",folderName="GhidossiGlobalEquity",email="reto.ghidossi@opencapital.ch")
	advisors[["MaggiDynamic"]] <- new("Advisor",name="MaggiDynamic",folderName="MaggiDynamic",email="maggi.sandro@")
	
	message <- messageFactory(fileName,advisors)
	
	string <- getMessageDate_time_from(message)
	checkEquals(string,"2012-05-09_14-22-24_GhidossiGlobalEquity")
	
}