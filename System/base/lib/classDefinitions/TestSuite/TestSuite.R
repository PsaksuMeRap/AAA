# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("TestSuite",representation(fileName="character",directory="character"))

setClass("TestSuiteParsed",representation(configLines="character",
				checkStrings="list"),contains="TestSuite")
