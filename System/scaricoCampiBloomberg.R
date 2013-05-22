# TODO: Add comment
# 
# Author: Claudio
###############################################################################


library("Rbbg",quietly=TRUE)

securities <- c(
		"XS0853679867 corp",
		"XS0842560640 corp",
		"XS0858803066 corp",
		"XS0843328526 corp",
		"FR0011348531 corp",
		"XS0848458179 corp",
		"XS0753190296 corp",
		"PTBSSGOE0009 corp"
)


fields <- c("ISSUER",
		"ID_BB_COMPANY",
		"ID_BB_ULTIMATE_PARENT_CO_NAME",
		"ID_BB_ULTIMATE_PARENT_CO",
		"ULT_PARENT_CNTRY_DOMICILE",
		"ULT_PARENT_CNTRY_INCORPORATION",
		"ULT_PARENT_CNTRY_OF_RISK"
)

conn <- blpConnect()

result <- bdp(conn, securities=securities, fields=fields)

blpDisconnect(conn)


