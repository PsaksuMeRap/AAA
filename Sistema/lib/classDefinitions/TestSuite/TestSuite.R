# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("TestSuite",representation(fileName="character",path="character"))

setClass("ParsedTestSuite",representation(configLines="character",
				checkStrings="character"),contains="TestSuite")
