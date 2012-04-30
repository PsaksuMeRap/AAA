# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("TestSuiteCollection",
		representation(
				name="character",
				directories="character",
				testFileRegexp="character",
				testSuitesParsed="list"
		)
)
