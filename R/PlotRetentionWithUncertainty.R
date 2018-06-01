
pool_summary2 <- readRDS(file='Outputs/UMR_RetentionEstimates_with_Uncertainty.rds')


#Exclude Pools 18-24
badpools<-paste('p', 18:24, sep='')
vars<-c('RNO3', 'RSPC', 'U', 'Vf')


pool_summary2[which(pool_summary2$Pool %in% badpools), which(names(pool_summary2) %in% vars)]<-NA

RNO3<-pool_summary2$RNO3
RSPC<-pool_summary2$RSPC
UNO3<-pool_summary2$U
VfNO3<-pool_summary2$Vf


# #######################
# Step X
# NO3 Retention
# #######################



png("E:/Dropbox/FLAME_MississippiRiver/N_retention_PerPool_ErrorBars2.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,2,0.5,0.25), oma=c(0,0,0,0))


barplot(RNO3, col="grey50", ylim=c(-.45, .650), las=1, space=0, yaxt="n")
error.bar(x=seq(0.5,26.5,1),
          y=RNO3, 
          upper.y=pool_summary2$RNO3_high-RNO3,
          lower.y=RNO3-pool_summary2$RNO3_low,
          col='black', lwd=.5)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot(RNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
axis(1, at=seq(1:length(Bardata$RNO3))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Bardata$RNO3))+0.5, par("usr")[3] - 0.04, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)
mtext(expression(paste(NO[3], " Retention (%)")), 2, 1)

text(x=mean(par('usr')[1:2]-3.2), y=par('usr')[3]-0.02, 'Production', pos=3, cex=cex)
text(x=mean(par('usr')[1:2]-3.2),  y=par('usr')[4]+0.02, 'Retention', pos=1, cex=cex)
arrows(y0=par('usr')[3:4]+c(.1,-.1), x0=mean(par('usr')[1:2]), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1:2]), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=.5, "Unreliable", cex=cex, pos=1)
text(x=22, y=.45, "retention", cex=cex, pos=1)
text(x=22, y=.4, "estimates", cex=cex, pos=1)
arrows(y0=-1.2, x0=17.5, y1=-1.2, x1=19, lwd=2, length=0.08)

box(which='plot')

dev.off()


#Storage difference as error
png("E:/Dropbox/FLAME_MississippiRiver/N_retention_PerPool_ErrorBars3.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,2,0.5,0.25), oma=c(0,0,0,0))


barplot(RNO3, col="grey50", ylim=c(-.45, .650), las=1, space=0, yaxt="n")
error.bar(x=seq(0.5,26.5,1)[which(RNO3!=pool_summary2$RNO3_low_storage)],
          y=RNO3[which(RNO3!=pool_summary2$RNO3_low_storage)], 
          upper.y=(pool_summary2$RNO3_high_storage-RNO3)[which(RNO3!=pool_summary2$RNO3_low_storage)],
          lower.y=(RNO3-pool_summary2$RNO3_low_storage)[which(RNO3!=pool_summary2$RNO3_low_storage)],
          col='black', lwd=.5)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot(RNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
axis(1, at=seq(1:length(Bardata$RNO3))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Bardata$RNO3))+0.5, par("usr")[3] - 0.04, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)
mtext(expression(paste(NO[3], " Retention (%)")), 2, 1)

text(x=mean(par('usr')[1:2]-3.2), y=par('usr')[3]-0.02, 'Production', pos=3, cex=cex)
text(x=mean(par('usr')[1:2]-3.2),  y=par('usr')[4]+0.02, 'Retention', pos=1, cex=cex)
arrows(y0=par('usr')[3:4]+c(.1,-.1), x0=mean(par('usr')[1:2]), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1:2]), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=.5, "Unreliable", cex=cex, pos=1)
text(x=22, y=.45, "retention", cex=cex, pos=1)
text(x=22, y=.4, "estimates", cex=cex, pos=1)
arrows(y0=-1.2, x0=17.5, y1=-1.2, x1=19, lwd=2, length=0.08)

box(which='plot')

dev.off()





#Average missing water as error
png("E:/Dropbox/FLAME_MississippiRiver/N_retention_PerPool_ErrorBars4.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,2,0.5,0.25), oma=c(0,0,0,0))


barplot(RNO3, col="grey50", ylim=c(-.45, .650), las=1, space=0, yaxt="n")
error.bar(x=seq(0.5,26.5,1),
          y=RNO3, 
          upper.y=(pool_summary2$RNO3_high_qdiff-RNO3),
          lower.y=(RNO3-pool_summary2$RNO3_low_qdiff),
          col='black', lwd=.5, length=0.02)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot(RNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
axis(1, at=seq(1:length(Bardata$RNO3))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Bardata$RNO3))+0.5, par("usr")[3] - 0.04, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)
mtext(expression(paste(NO[3], " Retention (%)")), 2, 1)

text(x=mean(par('usr')[1:2]-3.2), y=par('usr')[3]-0.02, 'Production', pos=3, cex=cex)
text(x=mean(par('usr')[1:2]-3.2),  y=par('usr')[4]+0.02, 'Retention', pos=1, cex=cex)
arrows(y0=par('usr')[3:4]+c(.1,-.1), x0=mean(par('usr')[1:2]), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1:2]), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=.5, "Unreliable", cex=cex, pos=1)
text(x=22, y=.45, "retention", cex=cex, pos=1)
text(x=22, y=.4, "estimates", cex=cex, pos=1)
arrows(y0=-1.2, x0=17.5, y1=-1.2, x1=19, lwd=2, length=0.08)

box(which='plot')

dev.off()




#Average missing water or missing water as error
png("E:/Dropbox/FLAME_MississippiRiver/N_retention_PerPool_ErrorBars5.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))


barplot(RNO3, col=c(rep("grey50",3), add.alpha('lightskyblue1', .4), rep("grey50",23)), ylim=c(-.45, .7), las=1, space=0, yaxt="n")
error.bar(x=seq(0.5,26.5,1),
          y=RNO3, 
          upper.y=(pool_summary2$RNO3_high_option-RNO3),
          lower.y=(RNO3-pool_summary2$RNO3_low_option),
          col='black', lwd=.5, length=0.02)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot(RNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
axis(1, at=seq(1:length(Bardata$RNO3))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Bardata$RNO3))+0.5, par("usr")[3] - 0.04, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)
mtext(expression(paste(NO[3], " Retention (%)")), 2, 1.5)

text(x=(par('usr')[1]+0.7), y=par('usr')[3]+0.05, 'Production', pos=4, cex=cex)
text(x=(par('usr')[1]+0.7),  y=par('usr')[4]-0.05, 'Retention', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(.1,-.1), x0=(par('usr')[1]+1), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1]+1), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-.075, "retention", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-.15, "estimates", cex=cex, pos=1)
# arrows(y0=-1.2, x0=17.5, y1=-1.2, x1=19, lwd=2, length=0.08)

box(which='plot')

dev.off()






png("E:/Dropbox/FLAME_MississippiRiver/SPC_retention_PerPool.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,2,0.5,0.25), oma=c(0,0,0,0))



barplot(RSPC, ylim=c(-.250, .250), las=1, space=0, yaxt="n", col=c(rep("grey50",3), add.alpha('lightskyblue1', .4), rep("grey50",23)))
# error.bar(x=seq(0.5,26.5,1),
#           y=RNO3, 
#           upper.y=pool_summary2$RNO3_high-RNO3,
#           lower.y=RNO3-pool_summary2$RNO3_low,
#           col='black', lwd=.5)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot(RSPC, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.2,.2, by=0.1), labels=seq(-20, 20, by=10))
axis(1, at=seq(1:length(RSPC))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(RSPC))+0.5, par("usr")[3] - 0.02, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)



mtext(expression(paste("SPC Retention (%)")), 2, 1)

text(x=(par('usr')[1]+0.7), y=par('usr')[3]+0.05, 'Missin high-SPC water source', pos=4, cex=cex)
text(x=(par('usr')[1]+0.7),  y=par('usr')[4]-0.05, 'Missin low-SPC water source', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(.06,-.06), x0=(par('usr')[1]+1), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1]+1), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "retention", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)
arrows(y0=-1.2, x0=17.5, y1=-1.2, x1=19, lwd=2, length=0.08)

box(which='plot')

dev.off()





# #######################
# Step X
# Uptake
# #######################




#Average missing water or missing water as error
png("E:/Dropbox/FLAME_MississippiRiver/N_uptake_PerPool.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))


barplot(U2[1:27], col=c(rep("grey50",3), add.alpha('lightskyblue1', .4), rep("grey50",23)), ylim=c(-400, 800), las=1, space=0, yaxt="n")
# error.bar(x=seq(0.5,26.5,1),
#           y=merge3$U[1:27],
#           upper.y=(merge3$UNO3_high_option[1:27]-U),
#           lower.y=(U-merge3$UNO3_low_option[1:27]),
#           col='black', lwd=.5, length=0.02)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot(U2, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1)
# axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
axis(1, at=seq(1:length(Bardata$RNO3))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Bardata$RNO3))+0.5, par("usr")[3] - 50, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)
mtext(expression(paste(NO[3], " Uptake (mg N m"^"-2", " d"^"-1", ")")), 2, 1.5)

text(x=(par('usr')[1]+4.7), y=par('usr')[3]+50, 'Production', pos=4, cex=cex)
text(x=(par('usr')[1]+4.7),  y=par('usr')[4]-50, 'Uptake', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(100,-100), x0=(par('usr')[1]+5), y1=par('usr')[3:4]+c(20,-20), x1=mean(par('usr')[1]+5), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "uptake", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)

box(which='plot')

dev.off()


#Average missing water or missing water as error
png("E:/Dropbox/FLAME_MississippiRiver/N_uptake_PerPool_errorbars.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))


barplot(U2[1:27], col=c(rep("grey50",3), add.alpha('lightskyblue1', .4), rep("grey50",23)), ylim=c(-600, 1300), las=1, space=0, yaxt="n")
error.bar(x=seq(0.5,26.5,1),
          y=merge3$U2[1:27],
          upper.y=(merge3$UNO3_high_option[1:27]-U2[1:27]),
          lower.y=(U2[1:27]-merge3$UNO3_low_option[1:27]),
          col='black', lwd=.5, length=0.02)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col="grey90", border=NA)
barplot(U2, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1)
# axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
axis(1, at=seq(1:length(Bardata$RNO3))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Bardata$RNO3))+0.5, par("usr")[3] - 50, labels = labels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)
mtext(expression(paste(NO[3], " Uptake (mg N m"^"-2", " d"^"-1", ")")), 2, 1.5)

text(x=(par('usr')[1]+4.7), y=par('usr')[3]+100, 'Production', pos=4, cex=cex)
text(x=(par('usr')[1]+4.7),  y=par('usr')[4]-100, 'Uptake', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(200,-200), x0=(par('usr')[1]+5), y1=par('usr')[3:4]+c(40,-40), x1=mean(par('usr')[1]+5), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "uptake", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)

box(which='plot')

dev.off()



