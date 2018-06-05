
library(zoo)

#Load flow data at dams
AugQDaily <- readRDS(file = "Outputs/UMR_Q_AugDailyDamsOrUSGS.rds")
AugQDaily_2DayAvg <- readRDS(file = "Outputs/UMR_Q_AugDaily_2DAVG_DamsOrUSGS.rds")
AugQDaily_3DayAvg <- readRDS(file = "Outputs/UMR_Q_AugDaily_3DAVG_DamsOrUSGS.rds")

#Load flow data at tribs
trib_df<-readRDS( file = "Outputs/UMRTribs_Q_DailyAug2015.rds")
InputChemistry <- readRDS(file = "Outputs/UMR_TribuaryChemistryAndQ.rds")
names(InputChemistry)[(grep("SpCond", names(InputChemistry)))]<-c('SPCuScm', 'SPCScm_t')


#Get Weather and pool area data
Weather<-readRDS('Outputs/UMR_Weather_EvapRate.rds')
merge2 <- readRDS(file = "Outputs/UMR_Pool_AreasAndVolumes.rds")

#directory for linear referenced flame data and dam locations
# locdir<-"E:/Dropbox/ArcGIS"


# ###############################################
# Step 1
# Load flame data and create rolling means
# ###############################################

#Load Flame data
data<-read.table(paste(locdir, '/UMR_AllDays_Route2.txt', sep=''), header=TRUE, sep="," ,skip=0)
data$riverkm<-data$MEAS/1000
data<-data[order(data$MEAS),]
data[data==0] <- NA
data$ltime<-as.POSIXct(data$ltime, format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")

names(data)[(grep("SpCnd", names(data)))]<-c('SPCuScm', 'SPCScm_t')

#Subset data for NO3, Turb, and SPC (no NAS)
NO3data<-data[!is.na(data$NITRATEM),]
NO3data$rollNO3<-rollmean(NO3data$NITRATEM, k=25, align='center', fill=NA)

Turbdata<-data[!is.na(data$TurbFNU),]
Turbdata$rollTurb<-rollmean(Turbdata$TurbFNU, k=10, align='center', fill=NA)

SPCdata<-data[!is.na(data$SPCuScm),]
SPCdata$rollSPC<-rollmean(SPCdata$SPCuScm, k=10, align='center', fill=NA)

# plot(NO3data$NITRATEM)
# lines(NO3data$rollNO3, type="l", col="red")
# 
# plot(Turbdata$TurbFNU)
# lines(Turbdata$rollTurb, type="l", col="red")
# 
# plot(SPCdata$SPCuScm)
# lines(SPCdata$rollSPC, type="l", col="red")


# ###############################################
# Step 2
# Load dam data and identify location along river
# ###############################################

dams<-read.csv(paste(locdir, '/DamsAlongRoute3.csv', sep=''), sep=",", header=TRUE)
dams$riverkm<-dams$MEAS/1000
dams<-dams[order(dams$MEAS, decreasing=FALSE),]
dams$name<-c('SAF-U', 'SAF-L', 1,2,3,4,5,'5A', 6,7,8,9,10,11,12,13,14,15,16,17,18, 19, 20, 21, 22, 24, 25, '26')

dams1<-dams[dams$riverkm>10,]
Pepindam<-as.data.frame(matrix(nrow=1, ncol=ncol(dams1)))
names(Pepindam)=names(dams1)
Pepindam[1,c(2,(ncol(Pepindam)-1):ncol(Pepindam))]<-c(148500 , 148.5, "Pepin")

dams2<-rbind(dams1, Pepindam)
dams2$riverkm<-as.numeric(dams2$riverkm)
dams2$MEAS<-as.numeric(dams2$MEAS)
dams2<-dams2[order(dams2$riverkm, decreasing=FALSE),]

# Create vector of locations along river
# Adjust dam 2 and 6 locations to river km 68 and 190.5, respectively. 
# Just downstream of dam where chemistry levels out
dam_km<-dams2$riverkm
dam_km[2]<-68
dam_km[6]<-190.5
dam_name<-dams2$name


#Indicate which pools (rows) have tributaries entering
InputChemistry$poolInterval<-findInterval(InputChemistry$riverkm, vec=c(0,dam_km) )


# #############################################
# Step 3
# Calculate flow weighted input and output NO3/Turb/SPC for each pool
# #############################################

# Make list and data frame to fill with data
flamedata_list<-list()
flamedata_list2<-flamedata_list
flamedata_list3<-flamedata_list
pool_summary2<-as.data.frame(matrix(nrow=length(dam_name), ncol=21))
names(pool_summary2)<-(c("Pool", "Date",  "RiverKM_start", "RiverKM_end","Pool_length", "NO3_start", "NO3_end", "dNO3", "RNO3", "Turb_start", "Turb_end", "dTurb",  "RTurb", "SPC_start", "SPC_end", "dSPC", "RSPC", "Q_MRin", 'Q_MRout', 'Q_Trib', 'Q_diff'))
pool_summary2$Date<-as.Date(pool_summary2$Date)

pool_summary3DayAvg<-pool_summary2

dam_nu<-1
for (dam_nu in 1:length(dam_km)){
  if (dam_nu==1){
    sub<-NO3data[NO3data$riverkm<dam_km[dam_nu],]
    sub2<-Turbdata[Turbdata$riverkm<dam_km[dam_nu],]
    sub3<-SPCdata[SPCdata$riverkm<dam_km[dam_nu],]
  } else {
    sub<-NO3data[NO3data$riverkm<dam_km[dam_nu] & NO3data$riverkm>dam_km[dam_nu-1], ]
    sub2<-Turbdata[Turbdata$riverkm<dam_km[dam_nu] & Turbdata$riverkm>dam_km[dam_nu-1], ] 
    sub3<-SPCdata[SPCdata$riverkm<dam_km[dam_nu] & SPCdata$riverkm>dam_km[dam_nu-1], ] 
  }
  flame_date<-ymd(median(as.Date(sub$ltime)))
  pool_summary2$Date[dam_nu]<-ymd(median(as.Date(sub$ltime)))
  
  flamedata_list[[dam_nu]]<-sub
  flamedata_list2[[dam_nu]]<-sub2
  flamedata_list3[[dam_nu]]<-sub3
  names(flamedata_list)[[dam_nu]]<-dam_name[dam_nu]
  names(flamedata_list2)[[dam_nu]]<-dam_name[dam_nu]
  names(flamedata_list3)[[dam_nu]]<-dam_name[dam_nu]
  
  #Distance
  pool_summary2$Pool[dam_nu]<-dam_name[dam_nu]
  pool_summary2$RiverKM_start[dam_nu]<-min(sub$riverkm, na.rm=T)
  pool_summary2$RiverKM_end[dam_nu]<-max(sub$riverkm, na.rm=T)
  pool_summary2$Pool_length[dam_nu]<-pool_summary2$RiverKM_end[dam_nu] - pool_summary2$RiverKM_start[dam_nu]
  
  #Discharge out (cms)
  MR_Q_out<-AugQDaily[AugQDaily$Date==flame_date,c(dam_name[dam_nu])]
  MR_Q_out_3d<-AugQDaily_3DayAvg[AugQDaily_3DayAvg$Date==flame_date,c(dam_name[dam_nu])]
  
  if (length(MR_Q_out)==1){
    pool_summary2$Q_MRout[dam_nu]<-MR_Q_out
    pool_summary3DayAvg$Q_MRout[dam_nu]<-MR_Q_out_3d}
  
  #Discharge in (cms)
  if (dam_nu==1) {
    MR_Q_in<-InputChemistry$Q[which(InputChemistry$Sample.Notes=='Mississippi River')]
    MR_Q_in_3d<-InputChemistry$Q_3dayAvg[which(InputChemistry$Sample.Notes=='Mississippi River')]
  } else {
    MR_Q_in<-AugQDaily[AugQDaily$Date==flame_date,c(dam_name[dam_nu-1])]
    MR_Q_in_3d<-AugQDaily_3DayAvg[AugQDaily_3DayAvg$Date==flame_date,c(dam_name[dam_nu-1])]
  }
  pool_summary2$Q_MRin[dam_nu]<- MR_Q_in
  pool_summary3DayAvg$Q_MRin[dam_nu]<- MR_Q_in_3d
  
  #Miss River Concentrations
  MR_NO3in<-median(sub$NITRATEM[1:10], na.rm=T)
  MR_Turbin<-median(sub2$TurbFNU[1:20], na.rm=T)
  MR_SPCin<-median(sub3$SPCuScm[1:20], na.rm=T)
  
  #Use Water Chem table for Pool 8 metrics
  #flame data include 3 sample paths since we sampled with pool 3 times over 2 days.
  if (dam_name[dam_nu]=='8'){
    MR_NO3in<-0.78
    MR_Turbin<-9.01
    MR_SPCin<-426.8
  }  
  
  #Reset Tribs to NA
  Trib_NO3in<- NA
  Trib_Turbin<- NA
  Trib_SPCin<- NA
  Trib_Q<- NA
  Trib_Q_3d<- NA
  
  # If Triburary exists in pool
  if (dam_nu %in% InputChemistry$poolInterval[which(InputChemistry$Sample.Notes!='Mississippi River')]){
    
    #Tributary Metrics
    Trib_NO3in<- InputChemistry$NITRATEMG[InputChemistry$poolInterval==dam_nu]
    Trib_Turbin<- InputChemistry$TurbFNU[InputChemistry$poolInterval==dam_nu]
    Trib_SPCin<- InputChemistry$SPCuScm[InputChemistry$poolInterval==dam_nu]
    Trib_Q<- InputChemistry$Q[InputChemistry$poolInterval==dam_nu]
    Trib_Q_3d<- InputChemistry$Q_3dayAvg[InputChemistry$poolInterval==dam_nu]
    
  }
  MR_Q_in_total<-sum(c(MR_Q_in,Trib_Q), na.rm=T)
  pool_summary2$Q_Trib[dam_nu]<-Trib_Q
  
  MR_Q_in_total_3d<-sum(c(MR_Q_in_3d,Trib_Q_3d), na.rm=T)
  pool_summary3DayAvg$Q_Trib[dam_nu]<-Trib_Q_3d
  
  #NO3 initial
  pool_summary2$NO3_start[dam_nu] <- sum(c(Trib_NO3in*Trib_Q, MR_NO3in*MR_Q_in), na.rm=T) / MR_Q_in_total
  pool_summary3DayAvg$NO3_start[dam_nu] <- sum(c(Trib_NO3in*Trib_Q_3d, MR_NO3in*MR_Q_in_3d), na.rm=T) / MR_Q_in_total_3d
  
  #Turb initial
  pool_summary2$Turb_start[dam_nu]<- sum(c(Trib_Turbin*Trib_Q, MR_Turbin*MR_Q_in), na.rm=T) / MR_Q_in_total
  pool_summary3DayAvg$Turb_start[dam_nu]<- sum(c(Trib_Turbin*Trib_Q_3d, MR_Turbin*MR_Q_in_3d), na.rm=T) / MR_Q_in_total_3d
  
  #SPC initial
  pool_summary2$SPC_start[dam_nu]<- sum(c(Trib_SPCin*Trib_Q, MR_SPCin*MR_Q_in), na.rm=T) / MR_Q_in_total
  pool_summary3DayAvg$SPC_start[dam_nu]<- sum(c(Trib_SPCin*Trib_Q_3d, MR_SPCin*MR_Q_in_3d), na.rm=T) / MR_Q_in_total_3d
  
  
  #NO3 final
  pool_summary2$NO3_end[dam_nu]<-median(sub$NITRATEM[(length(sub$NITRATEM)-9):length(sub$NITRATEM)], na.rm=T)
  pool_summary3DayAvg$NO3_end[dam_nu]<-median(sub$NITRATEM[(length(sub$NITRATEM)-9):length(sub$NITRATEM)], na.rm=T)
  #Turb final
  pool_summary2$Turb_end[dam_nu]<-median(sub2$TurbFNU[(length(sub2$TurbFNU)-19):length(sub2$TurbFNU)], na.rm=T)
  pool_summary3DayAvg$Turb_end[dam_nu]<-median(sub2$TurbFNU[(length(sub2$TurbFNU)-19):length(sub2$TurbFNU)], na.rm=T)
  #SPC final
  pool_summary2$SPC_end[dam_nu]<-median(sub3$SPCuScm[(length(sub3$SPCuScm)-19):length(sub3$SPCuScm)], na.rm=T)
  pool_summary3DayAvg$SPC_end[dam_nu]<-median(sub3$SPCuScm[(length(sub3$SPCuScm)-19):length(sub3$SPCuScm)], na.rm=T)
  
  if (dam_name[dam_nu]=='8'){
    pool_summary2$NO3_end[dam_nu]<-0.768
    pool_summary2$Turb_end[dam_nu]<-6.46
    pool_summary2$SPC_end[dam_nu]<-414.7 
    
    pool_summary3DayAvg$NO3_end[dam_nu]<-0.768
    pool_summary3DayAvg$Turb_end[dam_nu]<-6.46
    pool_summary3DayAvg$SPC_end[dam_nu]<-414.7 
  } 
  
  
  print(dam_name[dam_nu])
}


pool_summary2$Q_MRout[which(pool_summary2$Pool=='Pepin')]<-pool_summary2$Q_MRout[which(pool_summary2$Pool=='4')]-pool_summary2$Q_Trib[which(pool_summary2$Pool=='4')]

pool_summary3DayAvg$Q_MRout[which(pool_summary3DayAvg$Pool=='Pepin')]<-pool_summary3DayAvg$Q_MRout[which(pool_summary3DayAvg$Pool=='4')]-pool_summary3DayAvg$Q_Trib[which(pool_summary3DayAvg$Pool=='4')]

pool_summary3DayAvg[,1:5]<-pool_summary2[,1:5]

#Change name of pools
pool_summary2$Pool[pool_summary2$Pool!='Pepin'& !is.na(pool_summary2$Pool)]<-
  paste("p", pool_summary2$Pool[pool_summary2$Pool!='Pepin'& !is.na(pool_summary2$Pool)], sep="")
pool_summary3DayAvg$Pool[pool_summary3DayAvg$Pool!='Pepin'& !is.na(pool_summary3DayAvg$Pool)]<-
  paste("p", pool_summary3DayAvg$Pool[pool_summary3DayAvg$Pool!='Pepin'& !is.na(pool_summary3DayAvg$Pool)], sep="")

#add pool area and volume
pool_summary2$PoolArea<-merge2$TotalArea[match(pool_summary2$Pool, merge2$Pool)]
pool_summary2$Volume<-merge2$Volume[match(pool_summary2$Pool, merge2$Pool)]
pool_summary3DayAvg$PoolArea<-merge2$TotalArea[match(pool_summary3DayAvg$Pool, merge2$Pool)]
pool_summary3DayAvg$Volume<-merge2$Volume[match(pool_summary3DayAvg$Pool, merge2$Pool)]


# #############################################
# Step 4
# Calculate change in NO3/Turb/SPC for each pool
# Calculate difference and retention using vector match
# This assumes that input discharge is conservative
# #############################################


#NO3 change
pool_summary2$dNO3<- pool_summary2$NO3_start - pool_summary2$NO3_end
#NO3 Retention
pool_summary2$RNO3<-pool_summary2$dNO3/pool_summary2$NO3_start

#Turb change
pool_summary2$dTurb<- pool_summary2$Turb_start - pool_summary2$Turb_end
#Turb Retention
pool_summary2$RTurb<-pool_summary2$dTurb/pool_summary2$Turb_start

#SPC change
pool_summary2$dSPC<- pool_summary2$SPC_start - pool_summary2$SPC_end
#SPC Retention
pool_summary2$RSPC<-pool_summary2$dSPC/pool_summary2$SPC_start

pool_summary2$Q_Totalin<-rowSums(pool_summary2[c('Q_MRin', 'Q_Trib')], na.rm=T)

pool_summary2$Q_diff=pool_summary2$Q_Totalin-pool_summary2$Q_MRout
pool_summary2$Q_percent=pool_summary2$Q_diff/pool_summary2$Q_MRout


#Calculate other spiraling metrics
pool_summary2$U<-pool_summary2$dNO3*pool_summary2$Q_Totalin/pool_summary2$PoolArea*86.4
pool_summary2$Vf<-pool_summary2$U/pool_summary2$NO3_start*365/1000

pool_summary2$WRT_d<-pool_summary2$Volume/pool_summary2$Q_Totalin*(1000000/3600/24) #days
pool_summary2$Z_mean_m<-pool_summary2$Volume/pool_summary2$PoolArea
pool_summary2$H<-pool_summary2$Q_Totalin/pool_summary2$PoolArea*31.536

pool_summary2$dNload<-pool_summary2$dNO3*pool_summary2$Q_Totalin



#Retention model for 3Day average flows
#NO3 change
pool_summary3DayAvg$dNO3<- pool_summary3DayAvg$NO3_start - pool_summary3DayAvg$NO3_end
#NO3 Retention
pool_summary3DayAvg$RNO3<-pool_summary3DayAvg$dNO3/pool_summary3DayAvg$NO3_start

#Turb change
pool_summary3DayAvg$dTurb<- pool_summary3DayAvg$Turb_start - pool_summary3DayAvg$Turb_end
#Turb Retention
pool_summary3DayAvg$RTurb<-pool_summary3DayAvg$dTurb/pool_summary3DayAvg$Turb_start

#SPC change
pool_summary3DayAvg$dSPC<- pool_summary3DayAvg$SPC_start - pool_summary3DayAvg$SPC_end
#SPC Retention
pool_summary3DayAvg$RSPC<-pool_summary3DayAvg$dSPC/pool_summary3DayAvg$SPC_start

pool_summary3DayAvg$Q_Totalin<-rowSums(pool_summary3DayAvg[c('Q_MRin', 'Q_Trib')], na.rm=T)

pool_summary3DayAvg$Q_diff=pool_summary3DayAvg$Q_Totalin-pool_summary3DayAvg$Q_MRout
pool_summary3DayAvg$Q_percent=pool_summary3DayAvg$Q_diff/pool_summary3DayAvg$Q_MRout


pool_summary3DayAvg$U<-pool_summary3DayAvg$dNO3*pool_summary3DayAvg$Q_Totalin/pool_summary3DayAvg$PoolArea*86.4
pool_summary3DayAvg$Vf<-pool_summary3DayAvg$U/pool_summary3DayAvg$NO3_start*365/1000

pool_summary3DayAvg$WRT_d<-pool_summary3DayAvg$Volume/pool_summary3DayAvg$Q_Totalin*(1000000/3600/24) #days
pool_summary3DayAvg$Z_mean_m<-pool_summary3DayAvg$Volume/pool_summary3DayAvg$PoolArea
pool_summary3DayAvg$H<-pool_summary3DayAvg$Q_Totalin/pool_summary3DayAvg$PoolArea*31.536

pool_summary3DayAvg$dNload<-pool_summary3DayAvg$dNO3*pool_summary3DayAvg$Q_Totalin


# #############################################
# Step 5
# Estimate other water sources
# And change in storage
# #############################################


pool_summary2$EvapRate_mmPerDay<-Weather$E_mmPerDay[match(pool_summary2$Date, Weather$Date)]

pool_summary2$Q_Evap_m3PerS<-pool_summary2$EvapRate_mmPerDay*pool_summary2$PoolArea/86.4
pool_summary2$Q_Evap_Per<-pool_summary2$Q_Evap_m3PerS/pool_summary2$Q_Totalin

pool_summary2$Q_WWTP<-rep(11, nrow(pool_summary2))
pool_summary2$Q_WWTP_Per<-pool_summary2$Q_WWTP/pool_summary2$Q_Totalin

pool_summary2$Q_UnsampleTrib<-rep(41.91, nrow(pool_summary2))
pool_summary2$Q_UnsampleTrib_Per<-pool_summary2$Q_UnsampleTrib/pool_summary2$Q_Totalin


GW_rate_mPerS<-511/35.3147/sum(pool_summary2$PoolArea[pool_summary2$Pool%in% c('p7', 'p8')])/10^6

pool_summary2$Q_GW<-pool_summary2$PoolArea*(GW_rate_mPerS)*10^6
pool_summary2$Q_GW_Per<-pool_summary2$Q_GW/pool_summary2$Q_Totalin

delS<-abs(pool_summary2$Q_diff)
Potential<-rowSums(pool_summary2[c('Q_UnsampleTrib', 'Q_GW', 'Q_WWTP')]) - pool_summary2$Q_Evap_m3PerS
A<-rowMaxs(as.matrix(data.frame(delS, Potential)))

pool_summary2$PercentErrorWater<- A/pool_summary2$Q_MRout


print(pool_summary2)


#Subset table and save water budgets
water_summary<-pool_summary2[c('Pool', 'Q_MRin', 'Q_MRout', 'Q_Trib', 'Q_diff', 'Q_percent', 'Q_UnsampleTrib', 'Q_WWTP', 'Q_GW', 'Q_Evap_m3PerS', 'PercentErrorWater', 'Q_GW_Per', 'Q_Evap_Per', 'Q_WWTP_Per', 'Q_UnsampleTrib_Per', 'RiverKM_start', 'RiverKM_end')]


write.table(water_summary, file='Outputs/PoolWaterBudgets.csv', row.names=F, sep=',')
saveRDS(water_summary, file='Outputs/PoolWaterBudgets.rds')



# #############################################
# Step 5
# Re calculate change in NO3/Turb/SPC for each pool after adding additional water source
# Calculate difference and retention using vector match
# #############################################

#NO3 values used for unsampled water source
NO3min<-0
NO3max<-6.37

#Average amount of 'missing' water
AvgQDiff<-(-1)*mean(pool_summary2$Q_diff[which(pool_summary2$Pool != 'Pepin')])


# Multiple ways of bracketing retention

# ExtraQ<-rowSums(abs(pool_summary2[c('Q_diff', 'Q_WWTP', 'Q_UnsampleTrib')]))
# 
# #Error bars using high amount of extra water (GW + Trib + WWTP)
# HighN<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + ExtraQ*NO3max)/ (ExtraQ+pool_summary2$Q_Totalin-pool_summary2$Q_Evap_m3PerS)
# 
# LowN<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + ExtraQ*0)/ (ExtraQ+pool_summary2$Q_Totalin )
# 
# pool_summary2$dNO3_high<- (HighN - pool_summary2$NO3_end)
# pool_summary2$dNO3_low<- (LowN - pool_summary2$NO3_end)
# 
# pool_summary2$RNO3_high<-pool_summary2$dNO3_high/HighN
# pool_summary2$RNO3_low<-pool_summary2$dNO3_low/LowN

#Error bars using missing water as error
# HighN_storage<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + abs(pool_summary2$Q_diff*NO3max))/ (abs(pool_summary2$Q_diff)+pool_summary2$Q_Totalin)
# 
# LowN_storage<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + abs(pool_summary2$Q_diff*0))/ (abs(pool_summary2$Q_diff)+pool_summary2$Q_Totalin)
# 
# pool_summary2$dNO3_high_storage<- HighN_storage - pool_summary2$NO3_end
# pool_summary2$dNO3_low_storage<- LowN_storage - pool_summary2$NO3_end
# 
# pool_summary2$RNO3_high_storage<-pool_summary2$dNO3_high_storage/HighN_storage
# pool_summary2$RNO3_low_storage<-pool_summary2$dNO3_low_storage/LowN_storage
# 
# #Error bars using average missing water as error
# HighN_qdiff<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + AvgQDiff*NO3max)/ (AvgQDiff+pool_summary2$Q_Totalin)
# 
# LowN_qdiff<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + AvgQDiff*0)/ (AvgQDiff+pool_summary2$Q_Totalin)
# 
# pool_summary2$dNO3_high_qdiff<- HighN_qdiff - pool_summary2$NO3_end
# pool_summary2$dNO3_low_qdiff<- LowN_qdiff - pool_summary2$NO3_end
# 
# pool_summary2$RNO3_high_qdiff<-pool_summary2$dNO3_high_qdiff/HighN_qdiff
# pool_summary2$RNO3_low_qdiff<-pool_summary2$dNO3_low_qdiff/LowN_qdiff


# This is one used for the ERL submission on May 31, 2018
#Average missing water or actual missing
HighN_option<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))*NO3max)/ (rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))+pool_summary2$Q_Totalin)

LowN_option<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))*0)/ (rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))+pool_summary2$Q_Totalin)

pool_summary2$dNO3_high_option<- HighN_option - pool_summary2$NO3_end
pool_summary2$dNO3_low_option<- LowN_option - pool_summary2$NO3_end

pool_summary2$RNO3_high_option<-pool_summary2$dNO3_high_option/HighN_option
pool_summary2$RNO3_low_option<-pool_summary2$dNO3_low_option/LowN_option

pool_summary2$UNO3_high_option<-pool_summary2$dNO3_high_option*pool_summary2$Q_Totalin/pool_summary2$PoolArea*86.4
pool_summary2$UNO3_low_option<-pool_summary2$dNO3_low_option*pool_summary2$Q_Totalin/pool_summary2$PoolArea*86.4



#ouputs with uncertainty estimates
write.table(pool_summary2, file='Outputs/UMR_RetentionEstimates_with_Uncertainty.csv', row.names=F, sep=',')
saveRDS(pool_summary2, file='Outputs/UMR_RetentionEstimates_with_Uncertainty.rds')


