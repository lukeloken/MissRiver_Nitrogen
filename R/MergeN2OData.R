#Merge N2O Data with FLAME data Miss River
# Need to specicy with folder contains flame data and file name. Line 10-13


library(sp)
library(rgdal)
library(maptools)

# Select Flame Directory and File name to merge with N2O Data
# dir<-c("2015-10-13_Pool8")
#Select file name
file<-'UMR_AllDays_ChannelOnly_Centered'

#Read in N2O Data
setwd(locdir, '/Data/2015_UMR_AllDays')

N2Odata<-read.csv(paste(datadir, '/Data/2015_UMR_AllDays/N2O_Sat_2015_withTime.csv', sep=''), header=TRUE)
summary(N2Odata)

#Fix DateTime format
N2Odata$Date<-as.Date(paste("2015-", floor(N2Odata$DOY), sep=""), format="%Y-%j", tz="America/Chicago")
N2Odata$DateTime<-as.POSIXct(paste(N2Odata$Date, as.character(N2Odata$Time), sep=" "), format="%Y-%m-%d %H:%M:%S", tz="America/Chicago") 


#Only include columns of interest
N2Ovars <- c("Latitude", "Longitude", "DateTime", "N2O_sat_ratio" )
N2Odata <- N2Odata[N2Ovars]


#Get linear referenced UMR data
data<-read.table(paste(locdir, '/UMR_AllDays_Route2.txt', sep=''), header=TRUE, sep="," ,skip=0)
data$riverkm<-data$MEAS/1000
data<-data[order(data$MEAS),]
data[data==0] <- NA
data$ltime<-as.POSIXct(data$ltime, format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")

newdata<-merge(data, N2Odata, by.x="ltime", by.y="DateTime", all.x=TRUE )
summary(newdata)

# output files
write.table(newdata, "Outputs/UMRLinearReference_withN20.csv", sep=",", row.names=F, col.names=T)
saveRDS(newdata, file = "Outputs/UMRLinearReference_withN20.rds")


#Subset to only look at Lake Pepin
UpperPepinData<-newdata[newdata$riverkm < 135 & newdata$riverkm>112,]

png("E:/Dropbox/FLAME_MississippiRiver/LakePepin_threepanel.png", res=200, width=3,height=3.5, units="in")

par(mfrow=c(3,1))
par(mar=c(0.75,0.5,0,.5), oma=c(1.5,2,0.5,0))
pt.cex=0.5
cex=1
par(ps=8)
par(cex=cex)
par(mgp=c(3,.3,0))
par(tck=-.04)
xlim=c(113,134.5)
start<-114.7

plot(UpperPepinData$riverkm , UpperPepinData$NITRATEM,  xlim=xlim, type="p", pch=16, las=1, ylab="", xaxt="n", cex=pt.cex)
axis(1, labels=NA)
mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)
abline(v=start, lty=2)
legend("topright", inset=0.01, expression(paste(NO[3])), bty="n")


plot(UpperPepinData$riverkm , UpperPepinData$N2O_sat_ratio,  xlim=xlim, type="p", pch=16, las=1, ylab="", xaxt="n", cex=pt.cex)
axis(1, labels=NA)
mtext(expression(paste(N[2], "O (sat ratio)")), 2, 1.5, cex=cex)
abline(v=start, lty=2)
legend("topright", inset=0.01, expression(paste(N[2], "O", sep="")), bty="n")


plot(UpperPepinData$riverkm , UpperPepinData$CH4Sat/100, xlim=xlim, type="p", pch=16, las=1, ylab="", xaxt="n", cex=pt.cex)
axis(1, mgp=c(3,.1,0))
mtext(expression(paste(CH[4], " (sat ratio)")), 2, 1.5, cex=cex)
abline(v=start, lty=2)

text(start, par('usr')[3]+.85*(par('usr')[4]-par('usr')[3]), "Lake Pepin start", srt=90, pos=2, cex=0.75, offset=0.3)
# text(start, par('usr')[3]+.85*(par('usr')[4]-par('usr')[3]), "Lake Pepin start", srt=90, pos=2)
legend("topright", inset=0.01, expression(paste(CH[4])), bty="n", cex=cex)

mtext("River km", 1, 1, cex=cex)

dev.off()
