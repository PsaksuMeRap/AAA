# TODO: Add comment
# 
# Author: claudio
###############################################################################

plot.new()
# par(xaxt="n",yaxt="n",xaxs="i",yaxs="i")
# par(xaxs="i",yaxs="i")
plot.window(xlim=c(0,100), ylim=c(0,100))

x <- seq(0,100,by=10)
#for (i in x) abline(h=i)
#for (i in x) abline(v=i)

xleft <- x[-length(x)]

a <- data.frame(xleft=xleft,ybottom=xleft,xright=xleft+10,ytop=xleft+10)

#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
#		col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),
#		...)

k <- 0
for (i in xleft) {
	for (j in xleft) {
		rect(xleft=i,ybottom=j,xright=i+10,ytop=j+10,col=if(k %% 2) "red" else "blue")
		k <- k+1
	}
}

# http://www.jameskeirstead.ca/typography/changing-the-fonts-in-r-plots/
mtext("text to place", side=2, las=2,at=10,adj=1) 
axis(side=2,at=xleft[-1],labels=paste("label",1:9),pos=0)