# TODO: Add comment
# 
# Author: Claudio
###############################################################################




##First read in the arguments listed at the command line
args=(commandArgs(TRUE))

##args is now a list of character vectors
## Then cycle through each element of the list and evaluate the expressions.
eval(parse(text=args[[1]]))
eval(parse(text=args[[2]]))