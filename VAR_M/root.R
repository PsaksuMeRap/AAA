# TODO: Add comment
# 
# Author: claudio
###############################################################################



system("bash /home/claudio/Desktop/esterna_script")
command_send <- paste("mutt -s 'check' claudio.ortelli@usi.ch -a /home/claudio/Bacep/*",Sys.Date(),".log < /dev/null",sep="")
system(command_send)
system("bash /home/claudio/Desktop/interna_script")

