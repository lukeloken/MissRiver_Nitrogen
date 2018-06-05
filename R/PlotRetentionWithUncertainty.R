
source('R/AddAlpha.R')
source('R/PlotErrorBar.R')

# pool_summary2 <- readRDS(file='Outputs/UMR_RetentionEstimates_with_Uncertainty.rds')

pool_summary2 <- readRDS(file='Outputs/UMR_RetentionEstimates_with_Uncertainty.rds')

summary_df<-readRDS('Outputs/UMR_Pool_Areas.rds')


#X-axis labels
Plabels<-sub("p", "", pool_summary2$Pool)
Plabels<-sub("Pein", "Pepin", Plabels)

#colors for barplots
colors<-c('lightskyblue1', 'grey50', 'grey90', 'black')


#Exclude Pools 18-24 for most variables
badpools<-paste('p', 18:24, sep='')
goodvars<-c("Pool", "Date", "RiverKM_start", "RiverKM_end", "Pool_length", "PoolArea")

pool_summary2[which(pool_summary2$Pool %in% badpools), -which(names(pool_summary2) %in% goodvars)]<-NA

pool_summary2_witharea<-full_join(pool_summary2, summary_df, by='Pool')

RNO3<-pool_summary2$RNO3
RSPC<-pool_summary2$RSPC
UNO3<-pool_summary2$U
VfNO3<-pool_summary2$Vf



# #######################
# Step 1
# NO3 Retention
# #######################


png("Figures/N_retention_PerPool.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))


barplot(RNO3, col=c(rep(colors[2],3), add.alpha(colors[1], .4), rep(colors[2],23)), ylim=c(-.3, .6), las=1, space=0, yaxt="n")

polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col=colors[3], border=NA)
barplot(RNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
mtext(expression(paste(NO[3], " Retention (%)")), 2, 1.5)

axis(1, at=seq(1:length(Plabels))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Plabels))+0.5, par("usr")[3]-.04 , labels=Plabels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)

text(x=(par('usr')[1]+0.7), y=par('usr')[3]+0.05, 'Production', pos=4, cex=cex)
text(x=(par('usr')[1]+0.7),  y=par('usr')[4]-0.05, 'Retention', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(.1,-.1), x0=(par('usr')[1]+1), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1]+1), lwd=1.5, length=0.06)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "retention", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)

box(which='plot')

dev.off()


#Average missing water or missing water as error
png("Figures/N_retention_PerPool_ErrorBars5.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))


barplot(RNO3, col=c(rep(colors[2],3), add.alpha(colors[1], .4), rep(colors[2],23)), ylim=c(-.45, .7), las=1, space=0, yaxt="n")
error.bar(x=seq(1:length(Plabels))-0.5,
          y=RNO3, 
          upper.y=(pool_summary2$RNO3_high_option-RNO3),
          lower.y=(RNO3-pool_summary2$RNO3_low_option),
          col=colors[4], lwd=.5, length=0.02)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col=colors[3], border=NA)
barplot(RNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.25,.5, by=0.25), labels=seq(-25, 50, by=25))
mtext(expression(paste(NO[3], " Retention (%)")), 2, 1.5)

axis(1, at=seq(1:length(Plabels))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Plabels))+0.5, par("usr")[3]-.04 , labels=Plabels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)

text(x=(par('usr')[1]+0.7), y=par('usr')[3]+0.05, 'Production', pos=4, cex=cex)
text(x=(par('usr')[1]+0.7),  y=par('usr')[4]-0.05, 'Retention', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(.1,-.1), x0=(par('usr')[1]+1), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1]+1), lwd=1.5, length=0.06)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "retention", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)

box(which='plot')

dev.off()



# #######################
# Step 2
# SPC Retention
# #######################


png("Figures/SPC_retention_PerPool.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))

barplot(RSPC, ylim=c(-.250, .250), las=1, space=0, yaxt="n", col=c(rep(colors[2],3), add.alpha(colors[1], .4), rep(colors[2],23)))


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col=colors[3], border=NA)
barplot(RSPC, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1, at=seq(-.2,.2, by=0.1), labels=seq(-20, 20, by=10))
mtext(expression(paste("SPC Retention (%)")), 2, 1)

axis(1, at=seq(1:length(Plabels))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Plabels))+0.5, par("usr")[3]-.02 , labels=Plabels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)

text(x=(par('usr')[1]+0.7), y=par('usr')[3]+0.05, 'Missin high-SPC water source', pos=4, cex=cex)
text(x=(par('usr')[1]+0.7),  y=par('usr')[4]-0.05, 'Missin low-SPC water source', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(.06,-.06), x0=(par('usr')[1]+1), y1=par('usr')[3:4]+c(.02,-.02), x1=mean(par('usr')[1]+1), lwd=1.5, length=0.06)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "retention", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)

box(which='plot')

dev.off()


# #######################
# Step 3
# NO3 Uptake
# #######################


#Average missing water or missing water as error
png("Figures/N_uptake_PerPool.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))


barplot(UNO3, col=c(rep(colors[2],3), add.alpha(colors[1], .4), rep(colors[2],23)), ylim=c(-400, 800), las=1, space=0, yaxt="n")
# error.bar(x=seq(0.5,26.5,1),
#           y=merge3$U[1:27],
#           upper.y=(merge3$UNO3_high_option[1:27]-U),
#           lower.y=(U-merge3$UNO3_low_option[1:27]),
#           col='black', lwd=.5, length=0.02)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col=colors[3], border=NA)
barplot(UNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1)
mtext(expression(paste(NO[3], " Uptake (mg N m"^"-2", " d"^"-1", ")")), 2, 1.5)


axis(1, at=seq(1:length(Plabels))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Plabels))+0.5, par("usr")[3] - 50, labels=Plabels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)

text(x=(par('usr')[1]+4.7), y=par('usr')[3]+50, 'Production', pos=4, cex=cex)
text(x=(par('usr')[1]+4.7),  y=par('usr')[4]-50, 'Uptake', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(100,-100), x0=(par('usr')[1]+5), y1=par('usr')[3:4]+c(20,-20), x1=mean(par('usr')[1]+5), lwd=1.5, length=0.06)
# text(x=22.2, y=0.48, "High tributary", cex=cex, pos=3, offset=0.1)
# text(x=22.2, y=0.4, "flows", cex=cex, pos=3, offset=0.1)

text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "retention", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)

box(which='plot')

dev.off()


#Average missing water or missing water as error
png("Figures/N_uptake_PerPool_ErrorBars5.png", res=600, width=3.42,height=2.5197, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(mfrow=c(1,1))
par(tck=-0.02)
par(ps=8)

par(mar=c(2.25,3,0.5,0.25), oma=c(0,0,0,0))


barplot(UNO3, col=c(rep(colors[2],3), add.alpha(colors[1], .4), rep(colors[2],23)), ylim=c(-600, 1300), las=1, space=0, yaxt="n")
error.bar(x=seq(1:length(Plabels))-0.5,
          y=UNO3,
          upper.y=(pool_summary2$UNO3_high_option-UNO3),
          lower.y=(UNO3-pool_summary2$UNO3_low_option),
          col=colors[4], lwd=.5, length=0.02)


polygon( c(19, 19, 25, 25), c(par('usr')[3:4], par('usr')[4:3]), col=colors[3], border=NA)
barplot(UNO3, col=NA, las=1, space=0, add=T, yaxt='n')

abline(h=0, lwd=0.5, lty=3)
axis(2, mgp=c(3,0.4,0), las=1)
mtext(expression(paste(NO[3], " Uptake (mg N m"^"-2", " d"^"-1", ")")), 2, 1.5)

axis(1, at=seq(1:length(Plabels))-0.5, labels=NA, mgp=c(3,0,0), las=0)
text(seq(1:length(Plabels))+0.5, par("usr")[3] - 70, labels=Plabels, srt = 90, xpd = TRUE, cex=cex, pos=2)
mtext("Pool", 1, 1.2)

text(x=(par('usr')[1]+4.7), y=par('usr')[3]+100, 'Production', pos=4, cex=cex)
text(x=(par('usr')[1]+4.7),  y=par('usr')[4]-100, 'Uptake', pos=4, cex=cex)
arrows(y0=par('usr')[3:4]+c(200,-200), x0=(par('usr')[1]+5), y1=par('usr')[3:4]+c(40,-40), x1=mean(par('usr')[1]+5), lwd=1.5, length=0.06)


text(x=22, y=par('usr')[4], "Unreliable", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.08, "retention", cex=cex, pos=1)
text(x=22, y=par('usr')[4]-diff(par('usr')[3:4])*.16, "estimates", cex=cex, pos=1)

box(which='plot')

dev.off()


# #######################
# Step 4
# Drivers of NO3 retention
# #######################


png("Figures/N_retention_Drivers.png", res=400, width=4.2,height=6, units="in")

cex=0.8
par(cex=cex)
cexpt=1.5
ps=12
par(mfrow=c(3,2))
par(mar=c(3,1,0.5,0.5), oma=c(0,2.5,0,0))
par(mgp=c(3,.5,0))
par(tck=-0.03)

plot(pool_summary2_witharea$RNO3*100~ pool_summary2_witharea$PoolArea, las=1, pch=16, cex.axis=cex, cex=cexpt)
points(pool_summary2_witharea$RNO3[pool_summary2_witharea$Pool=='Pepin']*100~ pool_summary2_witharea$PoolArea[pool_summary2_witharea$Pool=='Pepin'], cex=cexpt, pch=21, col='black', bg=colors[1])
mtext(expression(paste('Total Area (', 'km'^'2', ')')),1,2, cex=cex)
axis(2, labels=NA, cex=cex)
abline(h=0)


legend('topleft',  c('Lake Pepin', 'Other Pools'), pch=c(21, 16), col='black', bty='o', pt.cex=cexpt, pt.bg=colors[1], box.col=NA)


box(which='plot')

plot(rowSums(data.frame(pool_summary2_witharea$BWc_Area, pool_summary2_witharea$I_Area))*100, pool_summary2_witharea$RNO3*100 , yaxt="n", pch=16, cex.axis=cex, cex=cexpt)

axis(2, labels=NA)
abline(h=0)
mtext(expression(paste('Non-Channel Area (%)')),1,2, cex=cex)

plot(pool_summary2_witharea$RNO3*100~ pool_summary2_witharea$NO3_start, las=1,pch=16, cex.axis=cex, cex=cexpt)
points(pool_summary2_witharea$RNO3[pool_summary2_witharea$Pool=='Pepin']*100~ pool_summary2_witharea$NO3_start[pool_summary2_witharea$Pool=='Pepin'], cex=cexpt, pch=21, col='black', bg=colors[1])
axis(2, labels=NA)
abline(h=0)
mtext(expression(paste('Incoming ', NO[3], " (mg N L"^"-1", ")")),1,2, cex=cex)

plot(pool_summary2_witharea$RTurb*100, pool_summary2_witharea$RNO3*100 , yaxt="n", pch=16, cex.axis=cex, cex=cexpt)
points(pool_summary2_witharea$RTurb[pool_summary2_witharea$Pool=='Pepin']*100, pool_summary2_witharea$RNO3[pool_summary2_witharea$Pool=='Pepin']*100, cex=cexpt, pch=21, col='black', bg=colors[1])
mtext(expression(paste('Turbidity Removal (%)')),1,2, cex=cex)
axis(2, labels=NA, cex=cex)
abline(h=0)

plot(pool_summary2_witharea$RNO3*100~ pool_summary2_witharea$H, las=1, pch=16, cex.axis=cex, cex=cexpt, xlim=c(100,3600), xaxt='n')
points(pool_summary2_witharea$RNO3[pool_summary2_witharea$Pool=='Pepin']*100~ pool_summary2_witharea$H[pool_summary2_witharea$Pool=='Pepin'], cex=cexpt, pch=21, col='black', bg=colors[1], xaxt='n')
axis(1, seq(0,3000,1000), cex.axis=cex)
mtext(expression(paste('Hydraulic Load (m yr'^"-1", ")")),1,2, cex=cex)
axis(2, labels=NA)
abline(h=0)

plot(pool_summary2_witharea$RNO3*100~ pool_summary2_witharea$WRT_d, yaxt="n", pch=16, cex.axis=cex, cex=cexpt)
points(pool_summary2_witharea$RNO3[pool_summary2_witharea$Pool=='Pepin']*100~ pool_summary2_witharea$WRT_d[pool_summary2_witharea$Pool=='Pepin'], cex=cexpt, pch=21, col='black', bg=colors[1])
mtext(expression(paste('Water Residence Time (d)')),1,2, cex=cex)
axis(2, labels=NA)
abline(h=0)
# legend("topleft", inset=0.01, c('Calculated', 'Modeled'), pch=c(16,1), bty="n")


mtext(expression(paste(NO[3], ' Retention (%)')),2,1, outer=T, cex=cex)

dev.off()

