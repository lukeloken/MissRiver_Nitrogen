
# plot LTRM NO3 data with flame data

source('R/ImageScale.R')

# Get Discharge Data
library(dataRetrieval)
# library(colorRamps)
# library(sp)
# library(gtools)
# library(sensorQC)

# get FLAME and LTER data for sample sites
AllMissMerged<-readRDS('Outputs/MissRiver_WaterChem_FlameLTER.rds')


# Get USGS gauge data for just Winona.
siteNumbers2 <-'05378500' # Winona (LD 5a) *** (pools 4-9)
startDate2 <- "2015-01-01"
endDate2 <- "2015-12-31"


dischargeWinona <- readNWISdv(siteNumbers2, parameterCd, startDate2, endDate2)
dischargeWinona <- renameNWISColumns(dischargeWinona)

# Convert to cubic meter per second 
dischargeWinona$Flow_cms<-dischargeWinona$Flow /35.3147

dischargeWinona$dateTime<-as.POSIXct(paste(dischargeWinona$Date, ' 12:00:00', sep=""), tz='America/Chicago')

# output files
write.table(dischargeWinona, "Outputs/UMR_Q_USGS_Winona2015.csv", sep=",", row.names=F, col.names=T)
saveRDS(dischargeWinona, file = "Outputs/UMR_Q_USGS_Winona2015.rds")


# Flow at Root River

#Triburary River gauge data
TsiteNumbers2<-c("05385000", #Root River Main
                "05385500") #Root River South


TribNames2<-c('Root River Main',
             'Root River South')
             

tribtable<-cbind(TribNames2, TsiteNumbers2)

trib_list2<-list()
for (trib in 1:length(TsiteNumbers2)){
  TDischarge2<-readNWISdv(TsiteNumbers2[trib], parameterCd, startDate2, endDate2)
  TDischarge2 <- renameNWISColumns(TDischarge2)
  TDischarge2$Flow_cms<-TDischarge2$Flow /35.3147
  
  trib_list2[[trib]]<-TDischarge2
  names(trib_list2)[trib]<-TribNames2[trib]
}

#Merge two Root River sites
Rootmerge<-merge(trib_list2[c('Root River Main')][[1]], trib_list2[c('Root River South')][[1]], by='Date')
Rootmerge$Flow_cms<-Rootmerge$Flow_cms.x+Rootmerge$Flow_cms.y

Root_final<-Rootmerge[c('Date', 'Flow_cms')]
Root_final$dateTime<-as.POSIXct(paste(Root_final$Date, ' 12:00:00', sep=""), tz='America/Chicago')

# output files
write.table(Root_final, "Outputs/Root_Q_2015.csv", sep=",", row.names=F, col.names=T)
saveRDS(Root_final, file = "Outputs/Root_Q_015.rds")

                       
#Get LTRMP NO3 data and plot
a<-read.csv('Data/N_turb_p8fss_2015.csv', header=T, stringsAsFactors = F)

a[,1]<-as.Date(a[,1], format="%m/%d/%Y")
a$Date.Time<-as.POSIXct(paste(a[,1], a[,2], sep=' '), format="%Y-%m-%d %H:%M", tz="America/Chicago")

a<-a[which(a$NOXQF!=64),]

sites<-unique(a$site)
sites<-sites[c(1:2,4,3,5:length(sites))]
names<-c('Lawrence Lake N', 'Lawrence Lake S', 'Target Lake', 'Stoddard Island',  'MC Center', 'MC East', 'MC West', 'MC Mean')

colors1<-colorRampPalette(c("blue4", "blue","slategray1", "orange", "brown4", "black"))(length(sites)+3)
colors<-colors1[2:(length(colors1)-1)]

pch<-c(rep(1, 4), rep(15, 3),NA)
lty<-c(rep(0,length(sites)), 1)

a$match<-match(a$site, sites, nomatch=NA)
a$col<-colors[a$match]
a$symbol<-pch[a$match]

main<-subset(a, match>=5)
main_avg<-as.data.frame(aggregate(main, by=list(main$date), FUN=mean, na.rm=T))

mainvars<-names(main)
main_avg2<-main_avg[mainvars]

jittertime<-as.POSIXct(jitter(as.numeric(a$Date.Time), 150), tz="America/Chicago", origin = "1970-01-01")
plot(jittertime~ a$Date.Time)
df<-data.frame(a$Date.Time, jittertime)
df$diff<-df[,1]-df[,2]

mean(as.numeric(df$diff))

#Plot LTRM and Discharge Data

png("Figures/LTRMP_NO3_2015.png", width=6, height=5, units='in', res=600)

cex=0.6
par(cex=cex, cex.axis=cex)
par(mar=c(2,2,0,0))
par(oma=c(.5,.5,.5,.5))
par(mgp=c(3,.5,0))
par(tck=c(-.03))
xlim=range(main_avg2$Date.Time)
layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(1), heights=c(6,4))

plot(jittertime, a$NOX, col=a$col, pch=a$symbol, lwd=2, xlab="", ylab="", las=1, xlim=xlim, cex=cex)
lines(main_avg2$Date.Time, main_avg2$NOX, col=colors[length(colors)], type="l", lwd=2)

legend("topright", inset=0.02, rev(c(names)), col=rev(colors), pch=rev(pch), bty = "o", ncol=1, lwd=2, lty=rev(lty), cex=cex)

mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)

plot(dischargeWinona$dateTime, dischargeWinona$Flow_cms, type="l", col='black', lty=1, lwd=1, ylab="", xlab="", xlim=xlim)

mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ")")), 2, 1.5, cex=cex)
mtext("2015", 1, -.5, cex=cex, outer=T)

dev.off()



png("Figures/LTRMP_NO3_MainChannelOnly2015.png", width=5, height=3, units='in', res=600)

cex=0.6
par(cex=cex, cex.axis=cex)
par(mar=c(2,2,0,0))
par(oma=c(.5,.5,.5,.5))
par(mgp=c(3,.5,0))
par(tck=c(-.03))
xlim=range(main_avg2$Date.Time)
layout(matrix(c(1,1), nrow=1, ncol=1), widths=c(1), heights=c(4))

plot(jittertime[which(a$site!='Lawrence Lake S')], a$NOX[which(a$site!='Lawrence Lake S')], col=a$col[which(a$site!='Lawrence Lake S')], pch=a$symbol[which(a$site!='Lawrence Lake S')], lwd=2, xlab="", ylab="", las=1, xlim=xlim, cex=cex)
lines(main_avg2$Date.Time, main_avg2$NOX, col=colors[length(colors)], type="l", lwd=2)

legend("topright", inset=0.02, rev(c(names[c(1,3:8)])), col=rev(colors[c(1,3:8)]), pch=rev(pch[c(1,3:8)]), bty = "o", ncol=1, lwd=2, lty=rev(lty[c(1,3:8)]), cex=cex)

mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)
mtext("2015", 1, -.5, cex=cex, outer=T)

dev.off()


#Prepare water chemistry data


dfNO3<-AllMissMerged[which(!is.na(AllMissMerged$`NO3 NO2`) & !is.na(AllMissMerged$NITRATEMG)),]
dfNO3$NO3_MG<-dfNO3$'NO3 NO2'/1000
plot(dfNO3$NITRATEMG~(dfNO3$NO3_MG))
NO3_model<-lm(dfNO3$NITRATEMG~ dfNO3$NO3_MG)
abline(NO3_model)
lines(c(-1, 10), c(-1, 10), col="red")
summary(NO3_model)
n<-nrow(dfNO3)

dfDOC<-AllMissMerged[which(!is.na(AllMissMerged$DOC) & !is.na(AllMissMerged$fDOMRFU)),]
plot(dfDOC$DOC~dfDOC$fDOMRFU)
ndoc<-nrow(dfDOC)
modeldoc<-lm(dfDOC$fDOMRFU~ dfDOC$DOC)

DFTN<-AllMissMerged[which(!is.na(AllMissMerged$`Total N (UF)`) & !is.na(AllMissMerged$`NO3 NO2`)),]
DFTN$NO3_MG<-DFTN$'NO3 NO2'/1000
DFTN$TN_MG<-DFTN$'Total N (UF)'/1000
plot(DFTN$NO3_MG~DFTN$TN_MG)
TN_model<-lm(DFTN$NO3_MG~DFTN$TN_MG)
abline(TN_model)
abline(0,1, col='red')
summary(TN_model)
n_TN<-nrow(DFTN)

# Plot FLAME (SUNA) vs LTER no3
B<-100 #Number of color breaks

# Turbidity color ramp
colors<-colorRampPalette(c("blue4", "blue","slategray1", "orange", "brown4", "black"))( 120 )[10:110]

dfNO3$Turbpoint<-log10(dfNO3$TurbFNU)
dfNO3$Col <- as.numeric(cut(dfNO3$Turbpoint,breaks = B))
dfNO3$Color<-colors[dfNO3$Col]
dfNO3<-dfNO3[order(dfNO3$TurbFNU),]

dfDOC$Turbpoint<-log10(dfDOC$TurbFNU)
dfDOC$Col <- as.numeric(cut(dfDOC$Turbpoint,breaks = B))
dfDOC$Color<-colors[dfDOC$Col]
dfDOC<-dfDOC[order(dfDOC$TurbFNU),]

DFTN$Turbpoint<-log10(DFTN$TurbFNU)
DFTN$Col <- as.numeric(cut(DFTN$Turbpoint,breaks = B))
DFTN$Color<-colors[DFTN$Col]
DFTN<-DFTN[order(DFTN$TurbFNU),]


#SUNA vs lab figure
png("Figures/LTER_SUNA_NO3.png", width=3, height=3.8, units='in', res=600)

cex=0.9
par(cex=cex, cex.axis=cex)
par(mar=c(2.5,2.5,0.5,0.5))
par(oma=rep(0,4))
par(mgp=c(3,.5,0))
par(tck=c(-.02))
lim=range(c(dfNO3$NITRATEMG, dfNO3$NO3_MG), na.rm=T)

layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(3), heights=c(3,.8))
breaks <- seq(min(dfNO3$Turbpoint, na.rm = TRUE), max(dfNO3$Turbpoint, na.rm = TRUE) ,length.out=100)
# par(mar=c(1,1,1,1))


# plot(dfNO3$NITRATEMG~dfNO3$NO3_MG, xlim=lim, ylim=lim, las=1, ylab="", xlab="", axes=F, col=dfNO3$Color, pch=16, cex=0.6+dfNO3$Turbpoint/1.5)
plot(dfNO3$NITRATEMG~dfNO3$NO3_MG, xlim=lim, ylim=lim, las=1, ylab="", xlab="", axes=F, col=dfNO3$Color, pch=16, cex=1.6)
box(which='plot')
axis(1, mgp=c(3,.3,0))
axis(2, las=1)

mtext(expression(paste('SUNA ', NO[3], " (mg N L"^"-1", ")")), 2, 1, cex=cex)
mtext(expression(paste('Lab ', NO[3], " (mg N L"^"-1", ")")), 1, 1.5, cex=cex)

abline(NO3_model)
lines(c(-1, 10), c(-1, 10), lty=2)

legend("topleft", inset=0.02, c("Least Squares", "1:1"), bty = "n", ncol=1, lty=c(1,2), cex=cex)
legend("bottomright", inset=0.02, c(paste("n=",n, sep="")), bty = "n",cex=cex)

#Add scale
par(mar=c(2,1,1,1), bg=NA, mgp=c(3, .1, 0))
image.scale((dfNO3$Turbpoint), col=colors[1:(B-1)], breaks=breaks-1e-8, axis.pos=1)
mtext(expression(paste(log[10], " Turbidity (FNU)", sep="")), 1, 1.1, cex=cex)
#abline(v=levs)
box()

dev.off()


#lab no3 vs TN
png("Figures/TN_vs_NO3.png", width=3, height=3.8, units='in', res=600)

cex=0.9
par(cex=cex, cex.axis=cex, bg='white')
par(mar=c(2.5,2.5,0.5,0.5))
par(oma=rep(0,4))
par(mgp=c(3,.5,0))
par(tck=c(-.02))
lim=range(c(DFTN$NO3_MG, DFTN$TN_MG), na.rm=T)

layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(3), heights=c(3,.8))
breaks <- seq(min(DFTN$Turbpoint, na.rm = TRUE), max(DFTN$Turbpoint, na.rm = TRUE) ,length.out=100)
# par(mar=c(1,1,1,1))


# plot(dfNO3$NITRATEMG~dfNO3$NO3_MG, xlim=lim, ylim=lim, las=1, ylab="", xlab="", axes=F, col=dfNO3$Color, pch=16, cex=0.6+dfNO3$Turbpoint/1.5)
plot(DFTN$NO3_MG~DFTN$TN_MG, xlim=lim, ylim=lim, las=1, ylab="", xlab="", axes=F, col=dfNO3$Color, pch=16, cex=1.6)
box(which='plot')
axis(1, mgp=c(3,.3,0))
axis(2, las=1)

mtext(expression(paste("Total Nitrogen (mg N L"^"-1", ")")), 1, 1.5, cex=cex)
mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1, cex=cex)

abline(TN_model)
lines(c(-1, 10), c(-1, 10), lty=2)

legend("topleft", inset=0.02, c("Least Squares", "1:1"), bty = "n", ncol=1, lty=c(1,2), cex=cex)
legend("bottomright", inset=0.02, c(paste("n=",n_TN, sep="")), bty = "n",cex=cex)

#Add scale
par(mar=c(2,1,1,1), bg=NA, mgp=c(3, .1, 0))
image.scale((dfNO3$Turbpoint), col=colors[1:(B-1)], breaks=breaks-1e-8, axis.pos=1)
mtext(expression(paste(log[10], " Turbidity (FNU)", sep="")), 1, 1.1, cex=cex)
#abline(v=levs)
box()

dev.off()

#ysi vs lab figure DOC-fdom
png("Figures/LTER_EXO_fdomDOC.png", width=3, height=3.8, units='in', res=600)

cex=0.9
par(cex=cex, cex.axis=cex, bg='white')
par(mar=c(2.5,2.5,0.5,0.5))
par(oma=rep(0,4))
par(mgp=c(3,.5,0))
par(tck=c(-.02))
xlim=range(dfDOC$DOC, na.rm=T)
ylim=range(dfDOC$fDOMRFU, na.rm=T)

layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(3), heights=c(3,.8))
breaks <- seq(min(dfDOC$Turbpoint, na.rm = TRUE), max(dfDOC$Turbpoint, na.rm = TRUE) ,length.out=100)
# par(mar=c(1,1,1,1))



plot(dfDOC$DOC, dfDOC$fDOMRFU, xlim=xlim, ylim=ylim, las=1, ylab="", xlab="", axes=F, col=dfDOC$Color, pch=16, cex=1.6)
box(which='plot')
axis(1, mgp=c(3,.3,0))
axis(2, las=1)

mtext(expression(paste('EXO fDOM (RFU)')), 2, 1.5, cex=cex)
mtext(expression(paste("Lab DOC (mg C L"^"-1", ")")), 1, 1.5, cex=cex)

abline(modeldoc)

legend("topleft", inset=0.02, c("Least Squares"), bty = "n", ncol=1, lty=c(1), cex=cex)
legend("bottomright", inset=0.02, c(paste("n=",ndoc, sep="")), bty = "n",cex=cex)

#Add scale
par(mar=c(2,1,1,1), bg=NA, mgp=c(3, .1, 0))
image.scale((dfDOC$Turbpoint), col=colors[1:(B-1)], breaks=breaks-1e-8, axis.pos=1)
mtext(expression(paste(log[10], " Turbidity (FNU)", sep="")), 1, 1.1, cex=cex)
#abline(v=levs)
box()

dev.off()


#ysi vs lab figure DOC-fdom
png("Figures/ChlA_TN_NO3_Turb.png", width=6, height=6, units='in', res=600)

cex=0.9
par(cex=cex, cex.axis=cex, bg='white')
par(mar=c(1.5,1.5,0.5,0.5))
par(oma=rep(0,4))
par(mgp=c(3,.5,0))
par(tck=c(-.02))


layout(matrix(c(1,1), nrow=1, ncol=1), widths=c(3), heights=c(3))

dfNO3$PN<-dfNO3$`Total N (UF)`-dfNO3$`Total N (F)`
dfNO3$PN[which(dfNO3$PN<0)]<-NA
dfNO3$logTurb<-log10(dfNO3$TurbFNU)
dfNO3$logChlA<-log10(dfNO3$ChlARFU)

# plot(dfNO3[c('TurbFNU', 'logTurb', 'ChlARFU', 'logChlA', 'PN', 'Total N (UF)', 'NITRATEMG')])
plot(dfNO3[c('TurbFNU',  'ChlARFU',  'PN', 'Total N (UF)', 'NITRATEMG', 'NH4')])

dev.off()


# Get FLAME data from sites that match LTRMP data

Flamesites<-unique(AllMissMerged$Sample.Notes)
allcsv2<-AllMissMerged[which(is.finite(AllMissMerged$NITRATEMG)),]

MCsites<-Flamesites[grep("7", Flamesites)]
MCdata<-allcsv2[which(allcsv2$Sample.Notes %in% MCsites),]

Lawsites<-Flamesites[grep("Law", Flamesites)]
Lawdata<-allcsv2[which(allcsv2$Sample.Notes %in% Lawsites),]

Tarsites<-Flamesites[grep("Tar", Flamesites)]
Tardata<-allcsv2[which(allcsv2$Sample.Notes %in% Tarsites),]

Tursites<-Flamesites[grep("Tur", Flamesites)]
Turdata<-allcsv2[which(allcsv2$Sample.Notes %in% Tursites),]

Consites<-Flamesites[grep("Cone", Flamesites)]
Condata<-allcsv2[which(allcsv2$Sample.Notes %in% Consites),]

Rensites<-Flamesites[grep("Reno", Flamesites)]
Rendata<-allcsv2[which(allcsv2$Sample.Notes %in% Rensites),]

subsites<-c("BW_LLN", "BW_TL", "BW_SI", "MC_C")
LLNdata<-a[a$site==subsites[1],]
TLdata<-a[a$site==subsites[2],]
SIdata<-a[a$site==subsites[3],]
MCCdata<-a[a$site==subsites[4],]

UMR_Dates <- as.POSIXct(c("2015-08-01 00:00:00", "2015-08-14 00:00:00"), tz="America/Chicago")



# Plot of NO3 over time in Pool 8.
# This will merge with Pool 8 maps (2015)
# png(file="Figures/FlameSites_LTRMP_NO3_2015.png", width=5, height=3, units='in', res=600)

png(file="Figures/FlameSites_LTRMP_NO3_2015.png", width=4.8, height=3, units='in', res=600)

MCcolor<-rgb(115,38, 0, max=255)
SCcolor<-rgb(168,112,0,  max=255)
SIcolor<-rgb(115, 223, 255,  max=255)
TLcolor<-rgb(115, 178, 255,  max=255)
TScolors<-c(MCcolor, SCcolor, SIcolor, TLcolor, 'dodgerblue4')
RRcolor<-c('black')
# TScolors<-c(colors[95], 'tan3', 'deepskyblue', 'dodgerblue', 'dodgerblue4')

cex=1
ptcex=0.7
par(ps=8)
par(cex=cex, cex.axis=cex)
par(mar=c(.5,2.5,0,0))
par(oma=c(1.5,1,0.5,5))
par(mgp=c(3,.3,0))
par(tck=c(-.03))
xlim=range(main_avg2$Date.Time)
ylim=range(c(TLdata$NOX, LLNdata$NOX, SIdata$NOX,main_avg2$NOX), na.rm=T)
flamedates<-unique(Lawdata$DateTime)
layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(1), heights=c(5,5))


plot(main_avg2$Date.Time, main_avg2$NOX, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, yaxt="n", xaxt="n", bty="L")
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=NA)
abline(v=flamedates, lty=2, col="grey40", lwd=1)

points(main_avg2$Date.Time, main_avg2$NOX, col=TScolors[1], type="o", lwd=1, cex=ptcex, ylim=ylim, pch=16)
axis(2, mgp=c(3,.3,0), las=1)
# axis(3, at=flamedates, mgp=c(3,.5,0), las=1, labels=NA, col="grey40")

points(TLdata$Date.Time, TLdata$NOX, col=TScolors[4], lwd=1, cex=ptcex, type="o", pch=16)
points(SIdata$Date.Time, SIdata$NOX, col=TScolors[3], lwd=1, cex=ptcex, type="o", pch=16)
points(LLNdata$Date.Time, LLNdata$NOX, col=TScolors[5], lwd=1, cex=ptcex, type="o", ylim=ylim, pch=16)

points(Tardata$DateTime, Tardata$NITRATEMG, col=TScolors[4], pch=8, cex=ptcex)
points(Lawdata$DateTime, Lawdata$NITRATEMG, col=TScolors[5], pch=8, cex=ptcex)
points(MCdata$DateTime, MCdata$NITRATEMG, col=TScolors[1], pch=8, cex=ptcex)
points(Turdata$DateTime, Turdata$NITRATEMG, col=TScolors[2], pch=8, cex=ptcex)

mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)

box(which='plot')


dimensions<-par('usr')

legend(dimensions[2]-2000000,dimensions[4]+0.1, c("Main Channel", "Side Channel", "Stoddard Island",  "Target Lake",  "Lawrence Lake"), col=TScolors, text.col= TScolors , bty = "n", ncol=1, cex=cex,  xpd=NA, y.intersp=0.7)

legend(dimensions[2]+500000,dimensions[4]-2.1, c("FLAME", "LTRM"), col='black', bty = "n", ncol=1, lty=c(0,1), pch=c(8, 16), pt.cex=ptcex, xpd=NA, y.intersp=0.7, seg.len=1.2, x.intersp=0.5)



plot(dischargeWinona$dateTime, dischargeWinona$Flow_cms, type="l", col=TScolors[1], lty=1, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n")
axis(2, mgp=c(3,.3,0), las=1, col.axis=TScolors[1], col.ticks=TScolors[1])
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=c('Mar 1', 'May 1', 'Jul 1', 'Sep 1', 'Nov 1'), mgp=c(3,.1,0))
mtext(expression(paste("Mississippi River")), 2, 2.5, cex=cex, col=TScolors[1])

mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 2, 1.5, cex=cex, col=TScolors[1])

#Plot Root River Discharge
par(new=T)

plot(Root_final$dateTime, Root_final$Flow_cms, type="l", col=RRcolor, lty=5, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n")
axis(4, mgp=c(3,.3,0), las=1,  col.axis=RRcolor,  col.ticks=RRcolor)
mtext(expression(paste("Root River")), 4, 1.25, cex=cex, col=RRcolor)
mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 4, 2.25, cex=cex, col=RRcolor)

mtext("2015", 1, 0.75, cex=cex, outer=F)
# mtext("2015", 1, 0.25, cex=cex, outer=T)

dev.off()




#Second plot. Remove site labels


png(file="Figures/FlameSites_LTRMP_NO3_2015_v2.png", width=4.5, height=3, units='in', res=600)

MCcolor<-rgb(115,38, 0, max=255)
SCcolor<-rgb(168,112,0,  max=255)
SIcolor<-rgb(115, 223, 255,  max=255)
TLcolor<-rgb(115, 178, 255,  max=255)
TScolors<-c(MCcolor, SCcolor, SIcolor, TLcolor, 'dodgerblue4')
RRcolor<-c('black')
# TScolors<-c(colors[95], 'tan3', 'deepskyblue', 'dodgerblue', 'dodgerblue4')

cex=1
ptcex=0.7
par(ps=8)
par(cex=cex, cex.axis=cex)
par(mar=c(.5,2.5,0,0))
par(oma=c(1.5,3,0.5,.5))
par(mgp=c(3,.3,0))
par(tck=c(-.03))
xlim=range(main_avg2$Date.Time)
ylim=c(0,2.8)
# c(TLdata$NOX, LLNdata$NOX, SIdata$NOX,main_avg2$NOX), na.rm=T)
flamedates<-unique(Lawdata$DateTime)
layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(1), heights=c(5,5))


plot(main_avg2$Date.Time, main_avg2$NOX, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, yaxt="n", xaxt="n", bty="L")
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=NA)
abline(v=flamedates, lty=2, col="grey40")

points(main_avg2$Date.Time, main_avg2$NOX, col=TScolors[1], type="o", lwd=1, cex=ptcex, ylim=ylim, pch=16)
axis(2, mgp=c(3,.3,0), las=1)
# axis(3, at=flamedates, mgp=c(3,.5,0), las=1, labels=NA, col="grey40")

points(TLdata$Date.Time, TLdata$NOX, col=TScolors[4], lwd=1, cex=ptcex, type="o", pch=16)
points(SIdata$Date.Time, SIdata$NOX, col=TScolors[3], lwd=1, cex=ptcex, type="o", pch=16)
points(LLNdata$Date.Time, LLNdata$NOX, col=TScolors[5], lwd=1, cex=ptcex, type="o", ylim=ylim, pch=16)

points(Tardata$DateTime, Tardata$NITRATEMG, col=TScolors[4], pch=8, cex=ptcex)
points(Lawdata$DateTime, Lawdata$NITRATEMG, col=TScolors[5], pch=8, cex=ptcex)
points(MCdata$DateTime, MCdata$NITRATEMG, col=TScolors[1], pch=8, cex=ptcex)
points(Turdata$DateTime, Turdata$NITRATEMG, col=TScolors[2], pch=8, cex=ptcex)

mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)

box(which='plot')


dimensions<-par('usr')

# legend(dimensions[2]-2000000,dimensions[4]+0.1, c("Main Channel", "Side Channel", "Stoddard Island",  "Target Lake",  "Lawrence Lake"), col=TScolors, text.col= TScolors , bty = "n", ncol=1, cex=cex,  xpd=NA, y.intersp=0.7)


legend(dimensions[1]+18500000, dimensions[4]+.15, c("FLAME", "LTRM"), col='black', bty = "n", ncol=1, lty=c(0,1), pch=c(8, 16), pt.cex=ptcex, xpd=NA, y.intersp=0.7, seg.len=1.2, x.intersp=0.5, bg='white', xjust=0, yjust=1)



plot(dischargeWinona$dateTime, dischargeWinona$Flow_cms, type="l", col=TScolors[1], lty=1, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n")
axis(2, mgp=c(3,.3,0), las=1, col.axis=TScolors[1], col.ticks=TScolors[1])
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=c('Mar 1', 'May 1', 'Jul 1', 'Sep 1', 'Nov 1'), mgp=c(3,.1,0))
mtext(expression(paste("Mississippi River")), 2, 1.5, cex=cex, col=TScolors[1])

# mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 2, 1.5, cex=cex, col=TScolors[1])

#Plot Root River Discharge
par(new=T)

plot(Root_final$dateTime, Root_final$Flow_cms, type="l", col=RRcolor, lty=5, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n", ylim=c(0, 180))
axis(2, line=2.5, mgp=c(3,.3,0), las=1,  col.axis=RRcolor,  col.ticks=RRcolor)
mtext(expression(paste("Root River")), 2, 3.75, cex=cex, col=RRcolor)
# mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 2, 4.75, cex=cex, col=RRcolor)

mtext("2015", 1, 0.75, cex=cex, outer=F)
# mtext("2015", 1, 0.25, cex=cex, outer=T)

dimensions<-par('usr')

text(dimensions[1]+17000000, dimensions[4]-20, expression(paste("Discharge (m"^"3", " s"^"-1", ')')), col='black', bty = "n", bg='white', pos=4)

dev.off()




#third plot, y-axis on right

png(file="Figures/FlameSites_LTRMP_NO3_2015_v3.png", width=4.5, height=3, units='in', res=600)

MCcolor<-rgb(115,38, 0, max=255)
SCcolor<-rgb(168,112,0,  max=255)
SIcolor<-rgb(115, 223, 255,  max=255)
TLcolor<-rgb(115, 178, 255,  max=255)
TScolors<-c(MCcolor, SCcolor, SIcolor, TLcolor, 'dodgerblue4')
RRcolor<-c('black')
# TScolors<-c(colors[95], 'tan3', 'deepskyblue', 'dodgerblue', 'dodgerblue4')

cex=1
ptcex=0.7
par(ps=8)
par(cex=cex, cex.axis=cex)
par(mar=c(.5,2.5,0,0))
par(oma=c(1.5,0.5,0.5,3))
par(mgp=c(3,.3,0))
par(tck=c(-.03))
xlim=range(main_avg2$Date.Time)
ylim=c(0,2.8)
# c(TLdata$NOX, LLNdata$NOX, SIdata$NOX,main_avg2$NOX), na.rm=T)
flamedates<-unique(Lawdata$DateTime)
layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(1), heights=c(5,5))


plot(main_avg2$Date.Time, main_avg2$NOX, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, yaxt="n", xaxt="n", bty="L")
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=NA)
abline(v=flamedates, lty=2, col="grey40")

points(main_avg2$Date.Time, main_avg2$NOX, col=TScolors[1], type="o", lwd=1, cex=ptcex, ylim=ylim, pch=16)
axis(4, mgp=c(3,.3,0), las=1)
# axis(3, at=flamedates, mgp=c(3,.5,0), las=1, labels=NA, col="grey40")

points(TLdata$Date.Time, TLdata$NOX, col=TScolors[4], lwd=1, cex=ptcex, type="o", pch=16)
points(SIdata$Date.Time, SIdata$NOX, col=TScolors[3], lwd=1, cex=ptcex, type="o", pch=16)
points(LLNdata$Date.Time, LLNdata$NOX, col=TScolors[5], lwd=1, cex=ptcex, type="o", ylim=ylim, pch=16)

points(Tardata$DateTime, Tardata$NITRATEMG, col=TScolors[4], pch=8, cex=ptcex)
points(Lawdata$DateTime, Lawdata$NITRATEMG, col=TScolors[5], pch=8, cex=ptcex)
points(MCdata$DateTime, MCdata$NITRATEMG, col=TScolors[1], pch=8, cex=ptcex)
points(Turdata$DateTime, Turdata$NITRATEMG, col=TScolors[2], pch=8, cex=ptcex)

mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 4, 1.5, cex=cex)

box(which='plot')


dimensions<-par('usr')

# legend(dimensions[2]-2000000,dimensions[4]+0.1, c("Main Channel", "Side Channel", "Stoddard Island",  "Target Lake",  "Lawrence Lake"), col=TScolors, text.col= TScolors , bty = "n", ncol=1, cex=cex,  xpd=NA, y.intersp=0.7)


legend(dimensions[1]+18500000, dimensions[4]+.15, c("FLAME", "LTRM"), col='black', bty = "n", ncol=1, lty=c(0,1), pch=c(8, 16), pt.cex=ptcex, xpd=NA, y.intersp=0.7, seg.len=1.2, x.intersp=0.5, bg='white', xjust=0, yjust=1)



plot(dischargeWinona$dateTime, dischargeWinona$Flow_cms, type="l", col=TScolors[1], lty=1, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n")
axis(2, mgp=c(3,.3,0), las=1, col.axis=TScolors[1], col.ticks=TScolors[1])
axis(1, at=seq(as.POSIXct('2015-03-01'), as.POSIXct('2015-11-01'), by='month')[c(1,3,5,7,9)], labels=c('Mar 1', 'May 1', 'Jul 1', 'Sep 1', 'Nov 1'), mgp=c(3,.1,0))
mtext(expression(paste("Mississippi River")), 2, 1.5, cex=cex, col=TScolors[1])

# mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 2, 1.5, cex=cex, col=TScolors[1])

#Plot Root River Discharge
par(new=T)

plot(Root_final$dateTime, Root_final$Flow_cms, type="l", col=RRcolor, lty=5, lwd=1, ylab="", xlab="", xlim=xlim, yaxt="n", xaxt="n", ylim=c(0, 180))
axis(4, line=0, mgp=c(3,.3,0), las=1,  col.axis=RRcolor,  col.ticks=RRcolor)
mtext(expression(paste("Root River")), 4, 1, cex=cex, col=RRcolor)
# mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ')')), 2, 4.75, cex=cex, col=RRcolor)

mtext("2015", 1, 0.75, cex=cex, outer=F)
# mtext("2015", 1, 0.25, cex=cex, outer=T)

dimensions<-par('usr')

text(dimensions[1]+17000000, dimensions[4]-20, expression(paste("Discharge (m"^"3", " s"^"-1", ')')), col='black', bty = "n", bg='white', pos=4)

dev.off()




