# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClass("FactorStringParsed",representation(criterium="character",negation="logical",values="character"))

# FactorStringParsed contains the parsed factorString like
# "security!:bond,equity" which give criterium="security",
# negation=TRUE, values=c("bond,equity")
# or a amount factorString like amount: >=250 EUR which returns
# criterium="amount", negation=FALSE, values=">=250 EUR"