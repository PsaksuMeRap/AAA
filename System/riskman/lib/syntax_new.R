# TODO: Add comment
# 
# Author: claudio
###############################################################################

# 1) one or more line starting with
filter ::
# or
directive ::

# 2) one final line starting with
should ::

# example
filter :: currency:USD,EUR + security:Bond
directive :: replace:Opzioni_su_azioni & groupBy:securityId
filter :: currency:USD
should :: = 0 EUR
