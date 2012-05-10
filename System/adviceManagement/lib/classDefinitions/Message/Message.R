# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("Message",representation(advisor="Advisor"),contains="namedList")
setClass("AdviceConfirmation",contains="Message")
setClass("NewAdvice",contains="Message")
setClass("PreComplianceResult",contains="Message")
setClass("PostComplianceResult",contains="Message")


