# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("Message",contains="list")
setClass("AdviceConfirmation",contains="Message")
setClass("NewAdvice",contains="Message")
setClass("PreComplianceResult",contains="Message")
setClass("PostComplianceResult",contains="Message")


