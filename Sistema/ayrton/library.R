# TODO: Add comment
# 
# Author: claudio
###############################################################################


# non dipende da nulla
source("./ayrton/lib/classDefinitions/AyrtonPosition/AyrtonPosition.R")
# non dipende da nulla
source("./ayrton/lib/classDefinitions/AyrtonPositions/AyrtonPositions.R")
# dipende da AyrtonPosition.R
source("./ayrton/lib/classDefinitions/AyrtonSecurity/AyrtonSecurity.R")
# dipende da AyrtonPosition
source("./ayrton/lib/classDefinitions/IdAyrton/IdAyrton.R")
# dipende da securities in Sistema
source("./ayrton/lib/methods/securityFactory/securityFactory.R")
# dipende da Position in Sistema
source("./ayrton/lib/methods/positionFactory/positionFactory.R")
# dipende da Positions in Sistema e dal metodo positionsFactory
source("./ayrton/lib/setMethod/positionsFactory/positionsFactory.R")
# dipende da AyrtonPosition, da idAyrton e dal metodo idFactory
source("./ayrton/lib/methods/idFactory/idFactory.R")



