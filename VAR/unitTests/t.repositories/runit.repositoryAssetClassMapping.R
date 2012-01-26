# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_repositoryAssetClassMapping <- function() {
	
	rep<- create_repositoryInstruments()
	
	checkEquals(rep$assetClassMapping[1,"ID_strumento"],1)
	checkEquals(rep$assetClassMapping[1,"ID_AAA"],138)	
}
