
# create the repositories environment
repositories <- new.env()

# import the base, riskman and ayrton libraries
source("./base/lib/library.R")

# set the systemOptions homedir and sourceCodeDir
# set the directory where the source code is installed (i.e. folders adviceManagement, ayrton, base, riskman)
systemOptions[["sourceCodeDir"]] <- sourceCodeDir
rm(sourceCodeDir)

# set the homeDir, i.e. the directory where i.e. the archive and the postOffice directories must be installed
systemOptions[["homeDir"]] <- homeDir
rm(homeDir)

# import the adviceManagement library
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","library.R"))

# change the working directory to the home directory
setwd(systemOptions[["homeDir"]])

# source the advisors
source(file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","lib","advisors.R"))

