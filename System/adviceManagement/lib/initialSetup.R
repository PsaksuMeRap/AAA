
# the initial working directory must be sourceCodeDir

# create the repositories environment
repositories <- new.env()

# import the base library
logger("Loading base library ...")
setwd(sourceCodeDir)
source(file.path(".","base","lib","library.R"))
loggerDone()

# remove the sourceCodeDir and homeDir variables
rm(sourceCodeDir)
rm(homeDir)

# import the ayrton library
logger("Loading ayrton library ...")
source(file.path(sys[["sourceCodeDir"]],"ayrton","lib","library.R"))
loggerDone()

# import the riskman library 
logger("Loading riskman library ...")
source(file.path(sys[["sourceCodeDir"]],"riskman","lib","library.R"))
loggerDone()

# import the adviceManagement library
logger("Loading adviceManagement library ...")
source(file.path(sys[["sourceCodeDir"]],"adviceManagement","lib","library.R"))
loggerDone()

# load the advisors
logger("Loading Advisors list ...")
source(file.path(sys[["sourceCodeDir"]],"adviceManagement","lib","advisors.R"))
loggerDone()

# change the working directory to the home directory
logger("Change working directory to homeDir ...")
setwd(sys[["homeDir"]])
loggerDone()