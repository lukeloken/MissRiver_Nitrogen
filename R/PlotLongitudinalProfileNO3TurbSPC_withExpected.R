
source("R/AddAlpha.R")

# #################################
# Load and maniuplate all data sets
# #################################

#Load linear referenced flame data
data<-read.table(paste(locdir, '/UMR_AllDays_Route2.txt', sep=''), header=TRUE, sep="," ,skip=0)
data$riverkm<-data$MEAS/1000
data<-data[order(data$MEAS, decreasing=FALSE),]
names(data)[(grep("SpCnd", names(data)))]<-c('SPCuScm', 'SPCScm_t')

#Subset to remove 0's and NA's
data2<-data[data$NITRATEM>0,]
data3<-data2[data2$TurbFNU>0,]


#Load expectations and Q
expected<-readRDS(file = "Outputs/UMR_ExpectedNO3Turb.rds")
TribQ<-expected
TribQ$name2<-c("MS", "MN", "SC", "Ch", "Rt", "WI", "MK", "Rk", "IA", "DM", "IL", "MO", 'OH')
TribQ<-TribQ[-which(TribQ$Sample.Notes %in% c("Mississippi River", "Maquoketa River")),]


#Use step function to create continuous expected concentration
stepNO3<-stepfun(x= expected$riverkm[-1], y=expected$NO3_Expect, f=0, right=F)
stepTurb<-stepfun(x= expected$riverkm[-1], y=log10(expected$Turb_Expect))
stepSPC<-stepfun(x= expected$riverkm[-1], y=expected$SPC_Expect)

stepNO3_3d<-stepfun(x= expected$riverkm[-1], y=expected$NO3_Expect_3d, f=0, right=F)
stepTurb_3d<-stepfun(x= expected$riverkm[-1], y=log10(expected$Turb_Expect_3d))
stepSPC_3d<-stepfun(x= expected$riverkm[-1], y=expected$SPC_Expect_3d)


#Load dam locations
dams<-read.csv(paste(locdir, '/DamsAlongRoute3.csv', sep=''), sep=",", header=TRUE)
dams$riverkm<-dams$MEAS/1000
dams<-dams[order(dams$MEAS, decreasing=FALSE),]
dams$name<-c('SAF-U', 'SAF-L', 1,2,3,4,5,'5a', 6,7,8,9,10,11,12,13,14,15,16,17,18, 19, 20, 21, 22, 24, 25, '26')
dams2<-dams[dams$riverkm>10,]


#Load tribs (Can probably use expected)
# TribQ<-readRDS("Outputs/UMR_TribuaryChemistryAndQ.rds")
# TribQ$name2<-c("MS", "MN", "SC", "Ch", "Rt", "WI", "MK", "Rk", "IA", "DM", "IL", "MO")
# TribQ<-TribQ[-which(TribQ$Sample.Notes %in% c("Mississippi River", "Maquoketa River")),]

# Load Dam discharge
poolQ<-readRDS(file="Outputs/PoolWaterBudgets.rds")


# ######################################
# Step 1 
# Calculate instanesous flow and N load
# ######################################

#Make a table of all dam locations and tribs
allriverkm<-unique(c(poolQ$RiverKM_end, expected$riverkm))
km<-allriverkm[order(allriverkm)]

df_qkm<-data.frame(km, Q=NA)

#First assign flow at the dams
df_qkm$Q[match(poolQ$RiverKM_end, df_qkm$km)]<-poolQ$Q_MRout

# Then add trib flow to preceeding dam flow
Q_at_tribs<-c(expected$Q[1], df_qkm$Q[match(expected$riverkm[-1], df_qkm$km)-1]+ expected$Q[-1])
df_qkm$Q[match(expected$riverkm, df_qkm$km)]<-Q_at_tribs


# Assign a flow to each measurement (probably can do this with an apply function)
row<-1
for (row in 1:nrow(data2)){
  x<-data2$riverkm[row]
  data2$Q_cumulative[row]<-df_qkm$Q[max(which(x>df_qkm$km))]
}

#Calculate load and convert units
data2$Nload_gPerS<-data2$Q_cumulative*data2$NITRATEM
data2$Nload_kgPerS<-data2$Nload_gPerS/1000


# ################
# Plots
# ################

# Plot observed (and expected) concentrations longitudinal profile
png("Figures/NO3-Turb-SPC-RiverKM_withExpected.png", res=200, width=6,height=7, units="in")
par(mfrow=c(3,1))
par(mar=c(1,2.5,.5,.5), oma=c(1.25,0,0,0))
par(mgp=c(3,.5,0))
par(tck=c(-.03))
cex=1
par(ps=8)
par(cex=cex, cex.axis=cex)
color2<-add.alpha('lightskyblue1', .4)
color3<-'gray65'

#Set up plot
plot(data2$riverkm,  data2$NITRATEM, type="n" , xlab="", ylab="", las=1, xlim=c(-10,1350), ylim=c(0,5.2),xaxs="i" , yaxs="i", cex=0.5, axes=F)
usr<-par('usr')

#Lake pepin polygon
polygon(c(114.7,	147.5, 147.5, 114.7), c(-1,-1,6,6), border=NA, col=color2)
text(137, usr[3]+.75*(usr[4]-usr[3]), "Lake Pepin", srt=90, cex=0.75, pos=3)

#Data and expectations
points(data2$riverkm,  data2$NITRATEM, type="p" , xlab="", ylab="", las=1, xaxs="i" , yaxs="i", cex=0.5, pch=16)
plot(stepNO3, xval=seq(0,max(data3$riverkm, na.rm=T), by=1), add=T,lwd=1.5, col=color3, do.points=F)

#axis
axis(1, mgp=c(3,.1,0))
axis(2, las=1)
mtext(expression(paste(NO[3], " (mg N L"^"-1", ")")), 2, 1.5, cex=cex)
box(which='plot')


#Plot Turbidity

#Set up plot
plot(data3$riverkm,  log10(data3$TurbFNU), type="n" , xlab="", ylab="", las=1, xlim=c(-10,1350),ylim=c(0, log10(160)), xaxs="i" , yaxs="i", cex=0.5, axes=F)
usr<-par('usr')

#Vertical lines at dams
abline(v=dams2$riverkm, col="red", lty=3)
text(dams2$riverkm, rep(c(.2, .1), nrow(dams2)/2), dams2$name, col="red", cex=.75, pos=2, offset=0.1)

#Lake Pepin
polygon(c(114.7,	147.5, 147.5, 114.7), c(-1,-1,3,3), border=NA, col=color2)
text(137, usr[3]+.75*(usr[4]-usr[3]), "Lake Pepin", srt=90, cex=0.75, pos=3)

#Data and expectations
points(data3$riverkm,  log10(data3$TurbFNU), type="p" , xlab="", ylab="", las=1, xlim=c(-10,1350),ylim=c(0, log10(160)), xaxs="i" , yaxs="i", cex=0.5,  pch=16)
plot(stepTurb, xval=seq(0,max(data3$riverkm, na.rm=T), by=1), add=T, lwd=1.5, col=color3, do.points=F)

#axis
axis(2, las=1)
mtext(expression(paste(log[10], " Turbidity (FNU)")), 2, 1.5, cex=cex)
axis(1, mgp=c(3,.1,0))
box(which='plot')


#Plot Conductivity

#set up plot
plot(data2$riverkm,  data2$SPCuScm, type="n" , xlab="", ylab="", las=1, xlim=c(-10,1350), ylim=c(310,650),xaxs="i" , yaxs="i", cex=0.5, axes=F)
usr<-par('usr')

#Vertical lines at tributaries
abline(v=TribQ$riverkm, col="blue", lty=5)
text(TribQ$riverkm, rep(640, nrow(TribQ)), TribQ$name2, col="blue", cex=0.75, pos=4, offset=0.1)

#Lake Pepin
polygon(c(114.7,	147.5, 147.5, 114.7), c(300,300,650,650), border=NA, col=color2)
text(137, usr[3]+.75*(usr[4]-usr[3]), "Lake Pepin", srt=90, cex=0.75, pos=3)

#Data and expectations
points(data2$riverkm,  data2$SPCuScm, type="p", xlab="", ylab="", las=1, xaxs="i" , yaxs="i", cex=0.5, pch=16)
plot(stepSPC, xval=seq(0,max(data3$riverkm, na.rm=T), by=1), add=T,lwd=1.5, col=color3, do.points=F)

#Axis
axis(1, mgp=c(3,.1,0))
axis(2, las=1)
mtext(expression(paste('Specific Conductivity (', mu, 'S cm'^'-1', ')')), 2, 1.5, cex=cex)
box(which='plot')

#Global axis and legend
mtext("River km", 1, 1, cex=cex)
axis(1, at=c(50,1250), labels=c("Upstream", "Downstream"), mgp=c(3,1,0), tck=0, cex.axis=cex)

legend("bottomright", inset=0.02, c("Tributaries", "Dams", "Expected"), col=c("blue", "red", color3), lty=c(5,3, 1), pch=NA, lwd=c(1,1,1.5), cex=cex, seg.len=1, bty='n', x.intersp=0.4)


dev.off()



# Plot NO3 load and cumulative discharge longitudinal profile
png("Figures/NO3Load-CumulativeQ.png", res=200, width=6,height=4, units="in")
par(mfrow=c(2,1))
par(mar=c(1,3.5,.5,.5), oma=c(1.25,0,0,0))
par(mgp=c(3,.5,0))
par(tck=c(-.03))
cex=1
par(ps=8)
par(cex=cex, cex.axis=cex)
color2<-add.alpha('lightskyblue1', .4)
color3<-'gray65'

#N load

#Set up plot
plot(data2$riverkm,  data2$Nload_kgPerS, type="n" , xlab="", ylab="", las=1, xlim=c(-10,1350), xaxs="i" , cex=0.5, axes=F)
usr<-par('usr')

#Vertical lines at tribs
abline(v=TribQ$riverkm, col="blue", lty=5)
text(TribQ$riverkm, rep(640, nrow(TribQ)), TribQ$name2, col="blue", cex=0.75, pos=4, offset=0.1)

#Lake Pepin
polygon(c(114.7,	147.5, 147.5, 114.7), c(par('usr')[3], par('usr')[3], par('usr')[4],par('usr')[4]), border=NA, col=color2)
text(137, usr[3]+.75*(usr[4]-usr[3]), "Lake Pepin", srt=90, cex=0.75, pos=3)

#Data
points(data2$riverkm,  data2$Nload_kgPerS, type="p" , xlab="", ylab="", las=1, xlim=c(-10,1350), xaxs="i" , yaxs="i", cex=0.5)

#Axis
axis(1, mgp=c(3,.1,0))
axis(2, las=1)
mtext(expression(paste(NO[3], " load (kg N s"^"-1", ")")), 2, 2, cex=cex)
box(which='plot')


#Plot Discharge

#set up plot
plot(data2$riverkm,  data2$Q_cumulative, type="n" , xlab="", ylab="", las=1, xlim=c(-10,1350), xaxs="i" , cex=0.5, axes=F)
usr<-par('usr')

#Vertical lines at dams
abline(v=dams2$riverkm[which(dams2$name!='Pepin')], col="red", lty=3)
text(dams2$riverkm[which(dams2$name!='Pepin')], rep(c(par('usr')[4]-300, par('usr')[4]-800), nrow(dams2)-1), dams2$name[which(dams2$name!='Pepin')], col="red", cex=.75, pos=2, offset=0.1)

#Lake Pepin
polygon(c(114.7,	147.5, 147.5, 114.7), c(par('usr')[3], par('usr')[3], par('usr')[4],par('usr')[4]), border=NA, col=color2)
text(137, usr[3]+.75*(usr[4]-usr[3]), "Lake Pepin", srt=90, cex=0.75, pos=3)

#Data
points(data2$riverkm,  data2$Q_cumulative, type="l" , xlab="", ylab="", las=1, xlim=c(-10,1350), xaxs="i" , yaxs="i", cex=0.5,  pch=16, lwd=2)

#Axis
axis(1, mgp=c(3,.1,0))
axis(2, las=1)
mtext(expression(paste('Discharge (m'^'3', ' s'^'-1', ')')), 2, 2, cex=cex)
box(which='plot')

#Outer axis and legend
mtext("River km", 1, 1, cex=cex)
axis(1, at=c(50,1250), labels=c("Upstream", "Downstream"), mgp=c(3,1,0), tck=0, cex.axis=cex)

legend("bottomright", inset=0.02, c("Tributaries", "Dams"), col=c("blue", "red"), lty=c(5,3), pch=NA, lwd=c(1,1), cex=cex, seg.len=1, bty='n', x.intersp=0.4)

dev.off()


# Plot just the upper UMR N load and discharge
png("Figures/NO3Load-CumulativeQ_upperUMR.png", res=200, width=6,height=4, units="in")
par(mfrow=c(2,1))
par(mar=c(1,3.5,.5,.5), oma=c(1.25,0,0,0))
par(mgp=c(3,.5,0))
par(tck=c(-.03))
cex=1
par(ps=8)
par(cex=cex, cex.axis=cex)
color2<-add.alpha('lightskyblue1', .4)
color3<-'gray65'

#N load

#Set up plot
plot(data2$riverkm,  data2$Nload_kgPerS, type="n" , xlab="", ylab="", las=1, xlim=c(-10,1350), ylim=c(0,3), xaxs="i" , cex=0.5, axes=F)
usr<-par('usr')

#tribs
abline(v=TribQ$riverkm, col="blue", lty=5)
text(TribQ$riverkm, rep(640, nrow(TribQ)), TribQ$name2, col="blue", cex=0.75, pos=4, offset=0.1)

#Lake pepin
polygon(c(114.7,	147.5, 147.5, 114.7), c(par('usr')[3], par('usr')[3], par('usr')[4],par('usr')[4]), border=NA, col=color2)
text(137, usr[3]+.75*(usr[4]-usr[3]), "Lake Pepin", srt=90, cex=0.75, pos=3)

#Data
points(data2$riverkm,  data2$Nload_kgPerS, type="p" , xlab="", ylab="", las=1, xlim=c(-10,1350), xaxs="i" , yaxs="i", cex=0.5)

#Axis
axis(1, mgp=c(3,.1,0))
axis(2, las=1)
mtext(expression(paste(NO[3], " load (kg N s"^"-1", ")")), 2, 2, cex=cex)
box(which='plot')

#Plot Discharge

#Set up plot
plot(data2$riverkm,  data2$Q_cumulative, type="n" , xlab="", ylab="", las=1, xlim=c(-10,1350), xaxs="i" , ylim=c(0,2000), cex=0.5, axes=F)
usr<-par('usr')

#Vertical lines at dams
abline(v=dams2$riverkm[which(dams2$name!='Pepin')], col="red", lty=3)
text(dams2$riverkm[which(dams2$name!='Pepin')], rep(c(par('usr')[4]-300, par('usr')[4]-800), nrow(dams2)-1), dams2$name[which(dams2$name!='Pepin')], col="red", cex=.75, pos=2, offset=0.1)

#Lake Pepin
polygon(c(114.7,	147.5, 147.5, 114.7), c(par('usr')[3], par('usr')[3], par('usr')[4],par('usr')[4]), border=NA, col=color2)
text(137, usr[3]+.75*(usr[4]-usr[3]), "Lake Pepin", srt=90, cex=0.75, pos=3)

#Data
points(data2$riverkm,  data2$Q_cumulative, type="l" , xlab="", ylab="", las=1, xlim=c(-10,1350), xaxs="i" , yaxs="i", cex=0.5,  pch=16, lwd=2)

#Axis
axis(1, mgp=c(3,.1,0))
axis(2, las=1)
mtext(expression(paste('Discharge (m'^'3', ' s'^'-1', ')')), 2, 2, cex=cex)
box(which='plot')

#Outer axis and legend
mtext("River km", 1, 1, cex=cex)
axis(1, at=c(50,1250), labels=c("Upstream", "Downstream"), mgp=c(3,1,0), tck=0, cex.axis=cex)

legend("bottomright", inset=0.02, c("Tributaries", "Dams"), col=c("blue", "red"), lty=c(5,3), pch=NA, lwd=c(1,1), cex=cex, seg.len=1, bty='n', x.intersp=0.4)

dev.off()


