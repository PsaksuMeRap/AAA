# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Amount",contains="numeric")

setMethod("formatC",
		signature(x = "Amount"),
		function (x, digits = NULL, width = NULL, format = NULL, flag = "", 
				mode = NULL, big.mark = "", big.interval = 3L, small.mark = "", 
				small.interval = 5L, decimal.mark = ".", preserve.width = "individual", 
				zero.print = NULL, drop0trailing = FALSE) 
		{	
			callNextMethod(x=unclass(x),width=width,big.mark = "'",decimal.mark = ".",
					format="f",digits=2)
		}
)

