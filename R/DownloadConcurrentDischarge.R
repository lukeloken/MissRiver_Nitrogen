
options(digits=4)
# library(foreign)

library(plyr)

library(dataRetrieval)
library(zoo)
library(lubridate)
library(matrixStats)

# #######################
# Step 1
# Get Dam Discharge Data
# #######################

# St. Paul District
DamQ<-read.csv('Data/USACE_Discharge_StPaulDams2015.csv', header=T, skip=6, stringsAsFactors = F)
DamQ$DateTime<-as.POSIXct(DamQ$X, format="%d%b%Y  %H%M", tz="America/Chicago")
DamQ<-DamQ[!is.na(DamQ$DateTime),]
DamQ$Date<-as.Date(DamQ$X, format="%d%b%Y")

DamQDaily1<-aggregate(DamQ[,3:13], by=list(DamQ$Date), FUN="mean")
names(DamQDaily1)[1]<-"Date"

# Rock Island District
DamQ_Rock<-read.csv('Data/USACE_Discharge_RockIslandDams2015.csv', header=T, skip=0, stringsAsFactors = F)
DamQ_Rock$Date<-as.Date(DamQ_Rock$Date, format="%Y-%m-%d")

# St. Louis District
DamQ_StLouis<-read.csv('Data/USACE_Discharge_StLouisDams2015.csv', header=T, skip=0, stringsAsFactors = F)
DamQ_StLouis$DateTime<-as.POSIXct(DamQ_StLouis$X, format="%d%b%Y  %H%M", tz="America/Chicago")
DamQ_StLouis<-DamQ_StLouis[!is.na(DamQ_StLouis$DateTime),]
DamQ_StLouis$Date<-as.Date(DamQ_StLouis$X, format="%d%b%Y")

# Merge districts
DamQDaily3<-aggregate(DamQ_StLouis[,2:4], by=list(DamQ_StLouis$Date), FUN="mean")
names(DamQDaily3)[1]<-"Date"

DamQDaily<-dplyr::full_join(DamQDaily1, DamQ_Rock, by='Date')
DamQDaily<-dplyr::full_join(DamQDaily, DamQDaily3, by='Date')


# Convert all flow records to cubic meters per second (cms)
DamQDaily[,2:ncol(DamQDaily)]<-DamQDaily[,2:ncol(DamQDaily)]/35.3147

# output files
write.table(DamQDaily, "Outputs/UMR_DamQDaily2015.csv", sep=",", row.names=F, col.names=T)
saveRDS(DamQDaily, file = "Outputs/UMR_DamQDaily2015.rds")


# ###############################################
# Step 2
# Determine river discharge during sampling campaign
# ###############################################

parameterCd <- c("00060") # Discharge
startDate <- "2015-07-30"
endDate <- "2015-08-31"

# Get USGS gauge data for all UMR stations.
siteNumbers<-c("05288500", # Mississippi River (Brooklyn Park, above Minneapolis)
               '05331000', # St. Paul *** (pools 2)
               '05331580', # Hastings (LD 2) *** (pools 2-LakePepin)
               '05344500', # Prescot (LD 3) - below St. Croix
               '05378500', # Winona (LD 5a) *** (pools 4-9)
               '05420500', # Clinton (LD 13) *** (pools 10-15)
               '05474500', # Keokuk (LD 19) *** (pools 16-19)
               '05587450', # At Grafton (LD 26)*** (pools 20-25)
               '07010000', # St. Louis ***
               '07020500', # Chester ***
               '07022000', # Thebes ***
               '370000089122601', # Above Cairo
               '365730089063001' # Below Cairo
) 

siteINFO<-readNWISsite(siteNumbers)

dischargeUnit <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)
dischargeUnit <- renameNWISColumns(dischargeUnit)

# Convert to cubic meter per second 
dischargeUnit$Flow_cms<-dischargeUnit$Flow /35.3147



# ==================================
# Get Tributary Water Chemistry and Discharge Data
# ==================================

#Triburary River gauge data
TsiteNumbers<-c("05288500",#Mississippi River
                "05330920",#Minnesota River
                "05344490",#St Croix River
                "05369500",#Chippewa River
                "05382000",#Black River Upper
                "05383075",#LaCrosse River
                "05385000",#Root River Main
                "05385500",#Root River South
                "05388250",#Upper Iowa River
                "05407000",#Wisconsin River
                "05418720",#Maquoketa River
                "05446500",#Rock River
                "05465700",#Iowa River
                "05490600",#Des Moines River
                "05586100",#Illinois River
                "06935965",#Missouri River
                "05355200",#Cannon River
                "05374900",#Zumbro River
                "05422000",#Wapsipinicon River
                "05474000",#Skunk River
                "05508000",#Salt River
                "05595000"#Kaskaskia River
)


TribNames<-c('Mississippi River',
             'Minnesota River',
             'St. Croix River',
             'Chippewa River',
             'Black River Upper',
             'LaCrosse River',
             'Root River Main',
             'Root River South',
             'Upper Iowa River',
             'Wisconsin River',
             'Maquoketa River',
             'Rock River',
             'Iowa River Confluence',
             'Des Moines River',
             'Illinois River',
             'Missouri River', 
             'Cannon River',
             'Zumbro River',
             'Wapsipinicon River',
             'Skunk River',
             'Salt River',
             'Kaskaskia River'
             )

tribtable<-cbind(TribNames, TsiteNumbers)

trib_list<-list()
for (trib in 1:length(TsiteNumbers)){
  TDischarge<-readNWISdv(TsiteNumbers[trib], parameterCd, startDate, endDate)
  TDischarge <- renameNWISColumns(TDischarge)
  TDischarge$Flow_cms<-TDischarge$Flow /35.3147
  
  trib_list[[trib]]<-TDischarge
  names(trib_list)[trib]<-TribNames[trib]
}

Rootmerge<-merge(trib_list[c('Root River Main')][[1]], trib_list[c('Root River South')][[1]], by='Date')
Rootmerge$Flow_cms<-Rootmerge$Flow_cms.x+Rootmerge$Flow_cms.y
trib_list[[length(trib_list)+1]]<-Rootmerge
names(trib_list)[[length(trib_list)]]<-'Root River'

#For Zumbro River use august mean
Z_df<-trib_list[[1]]
Z_df$site_no<-rep('05374900', length(Z_df$site_no))
Z_df$Flow<-rep(707, length(Z_df$Flow))
Z_df$Flow_cd<-rep('AugMean', length(Z_df$Flow))
Z_df$Flow_cms<-rep(707/35.3147, length(Z_df$Flow))
trib_list[['Zumbro River']]<-Z_df

max_TribQ<-max(c(trib_list[['Upper Iowa River']]$Flow_cms[-(1:2)], trib_list[['Cannon River']]$Flow_cms[-(1:2)], trib_list[['Zumbro River']]$Flow_cms[-(1:2)], trib_list[['Wapsipinicon River']]$Flow_cms)[-(1:2)], na.rm=T)

setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(trib_list, file = "UMR_TribsQDailyAug2015.rds")

#Get Tributary locations
setwd("E:/Dropbox/ArcGIS")
tribs<-read.csv('TribsAlongRoute.csv', sep=",", header=TRUE, stringsAsFactors = F)
tribs$riverkm<-tribs$MEAS/1000
tribs<-tribs[order(tribs$MEAS, decreasing=FALSE),]

tribs$name2<-c("MN", "SC", "Ch", "Bl", "Rt", "WI", "Rk", "IA", "DM", "IL", "MO", "OH")
tribs2<-tribs[tribs$NAME!="Black River",]
tribs_add<-tribs2[1,]
tribs_add[1,]<-NA
tribs_add[1,c('riverkm')]<-c(491)
tribs_add[1, c('NAME', 'name2')]<-c("Maquoketa River", "MA")
tribs2<-rbind(tribs2, tribs_add)


setwd('E:/Dropbox/FLAME_MississippiRiver/Data/2015_UMR_AllDays')
TribChemistry<-read.csv('UMR2015_AllWaterChemSamples.csv', header=T, stringsAsFactors = F)

TribChemistry2<-  TribChemistry[TribChemistry$Sample.Notes %in%  c(names(trib_list), 'UofM-Bridge') ,]
TribChemistry2$DateTime<-as.Date(TribChemistry2$DateTime, format="%m/%d/%Y")
TribChemistry2$Sample.Notes[which(TribChemistry2$Sample.Notes=='UofM-Bridge')]<-'Mississippi River'

TribChemistry2$Q<-NA
TribChemistry2$Q_2dayAvg<-NA
TribChemistry2$Q_3dayAvg<-NA
sample=1
for (sample in 1:nrow(TribChemistry2)){
  site<-TribChemistry2$Sample.Notes[sample]
  date<-TribChemistry2$DateTime[sample]
  table<-trib_list[[site]]
  TribChemistry2$Q[sample]<-table$Flow_cms[table$Date==date]
  TribChemistry2$Q_2dayAvg[sample]<-mean(table$Flow_cms[table$Date<=date & table$Date>=(date-1)], na.rm=T)
  TribChemistry2$Q_3dayAvg[sample]<-mean(table$Flow_cms[table$Date<=date & table$Date>=(date-2)], na.rm=T)
  
}

TribChemistry2$Sample.Notes<-sub("St.", "Saint", TribChemistry2$Sample.Notes)
TribChemistry2$Sample.Notes<-sub("River Confluence", "River", TribChemistry2$Sample.Notes)

Inputs<-intersect(c(tribs2$NAME, 'Mississippi River'), TribChemistry2$Sample.Notes)
InputChemistry<-TribChemistry2[TribChemistry2$Sample.Notes %in% Inputs,]

InputChemistry$riverkm[match(Inputs,InputChemistry$Sample.Notes)]<-tribs2$riverkm[match(Inputs,tribs2$NAME)]
InputChemistry$riverkm[which(InputChemistry$Sample.Notes=='Mississippi River')]<-0

setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(InputChemistry, file = "Data/UMR_TribuaryChemistryAndQ.rds")
write.table(InputChemistry, file = "Data/UMR_TribuaryChemistryAndQ.csv", row.names=F, sep=',')


#Make Table of flow for each pool

AugQDaily<-subset(DamQDaily, Date>=startDate & Date<=endDate)
names(AugQDaily)<-sub("DAM", "", names(AugQDaily))

#There are flows at their respetive dams, so the pool outlet!
PoolSAF<-dischargeUnit[dischargeUnit$site_no=='05288500', c('Date', 'Flow_cms')]
Pool1<-data.frame(Date=dischargeUnit[dischargeUnit$site_no=='05331000', c('Date')], Flow_cms=dischargeUnit[dischargeUnit$site_no=='05331000', c('Flow_cms')]-trib_list[['Minnesota River']]$Flow_cms)

Pool2<-dischargeUnit[dischargeUnit$site_no=='05331580', c('Date', 'Flow_cms')]
05331580
Pool3<-dischargeUnit[dischargeUnit$site_no=='05344500', c('Date', 'Flow_cms')]
Pool5A<-dischargeUnit[dischargeUnit$site_no=='05378500', c('Date', 'Flow_cms')]
Pool13<-dischargeUnit[dischargeUnit$site_no=='05420500', c('Date', 'Flow_cms')]
Pool19<-dischargeUnit[dischargeUnit$site_no=='05474500', c('Date', 'Flow_cms')]
Pool26<-dischargeUnit[dischargeUnit$site_no=='05587450', c('Date', 'Flow_cms')]


AugQDaily$'1' <-Pool1$Flow_cms[match(AugQDaily$Date, Pool1$Date)]
AugQDaily$'2' <-Pool2$Flow_cms[match(AugQDaily$Date, Pool2$Date)]
AugQDaily$'3' <-Pool3$Flow_cms[match(AugQDaily$Date, Pool3$Date)]
AugQDaily$'5A' <-Pool5A$Flow_cms[match(AugQDaily$Date, Pool5A$Date)]
AugQDaily$'13' <-Pool13$Flow_cms[match(AugQDaily$Date, Pool13$Date)]
AugQDaily$'19' <-Pool19$Flow_cms[match(AugQDaily$Date, Pool19$Date)]
AugQDaily$'26' <-Pool26$Flow_cms[match(AugQDaily$Date, Pool26$Date)]

AugQDaily$Pepin<-AugQDaily$'3'
AugQDaily$'15'<-AugQDaily$'14'

setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(AugQDaily, file = "UMR_AugQDaily.rds")

#Create another table of rolling means flows
Q_Short<-subset(DamQDaily, Date>='2015-07-30' & Date<=endDate)
names(Q_Short)<-sub("DAM", "", names(Q_Short))

Q_Short$'1' <-Pool1$Flow_cms[match(Q_Short$Date, Pool1$Date)]
Q_Short$'2' <-Pool2$Flow_cms[match(Q_Short$Date, Pool2$Date)]
Q_Short$'3' <-Pool3$Flow_cms[match(Q_Short$Date, Pool3$Date)]
Q_Short$'5A' <-Pool5A$Flow_cms[match(Q_Short$Date, Pool5A$Date)]
Q_Short$'13' <-Pool13$Flow_cms[match(Q_Short$Date, Pool13$Date)]
Q_Short$'19' <-Pool19$Flow_cms[match(Q_Short$Date, Pool19$Date)]
Q_Short$'26' <-Pool26$Flow_cms[match(Q_Short$Date, Pool26$Date)]

Q_Short$Pepin<-Q_Short$'3'
Q_Short$'15'<-Q_Short$'14'

Q_2DayAvg<-data.frame(Q_Short$Date, sapply(Q_Short[,2:28], rollmean, k=2, align='right', fill=NA))
Q_3DayAvg<-data.frame(Q_Short$Date, sapply(Q_Short[,2:28], rollmean, k=3, align='right', fill=NA))

names(Q_2DayAvg)<-names(Q_Short)
names(Q_3DayAvg)<-names(Q_Short)


# =================================
# Step 4
# Calculate Change in NO3/Turb for each pool
# Flow weighted
# =================================

#Get Weather Data
Weather<-readRDS('Data/UMR_Weather_EvapRate.rds')

#Load Flame data
setwd("E:/Dropbox/ArcGIS")

data<-read.table('UMR_AllDays_Route2.txt', header=TRUE, sep="," ,skip=0)
data$riverkm<-data$MEAS/1000
data<-data[order(data$MEAS),]
data[data==0] <- NA
data$ltime<-as.POSIXct(data$ltime, format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")

NO3data<-data[!is.na(data$NITRATEM),]
NO3data$rollNO3<-rollmean(NO3data$NITRATEM, k=25, align='center', fill=NA)

Turbdata<-data[!is.na(data$TurbFNU),]
Turbdata$rollTurb<-rollmean(Turbdata$TurbFNU, k=10, align='center', fill=NA)

SPCdata<-data[!is.na(data$SpCndÂµS),]
SPCdata$rollSPC<-rollmean(SPCdata$SpCndÂµS, k=10, align='center', fill=NA)


plot(NO3data$NITRATEM)
lines(NO3data$rollNO3, type="l", col="red")

plot(SPCdata$SpCndÂµS)
lines(SPCdata$rollSPC, type="l", col="red")


#Load dam data
dams<-read.csv('DamsAlongRoute3.csv', sep=",", header=TRUE)
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

dam_km<-dams2$riverkm
#Adjust dam 2 location to river km 68. Just downstream of dam where chemistry levels out
dam_km[2]<-68
dam_km[6]<-190.5
dam_name<-dams2$name

dams$name



#Indicate which rows have tributaries entering
InputChemistry$poolInterval<-findInterval(InputChemistry$riverkm, vec=c(0,dam_km) )

# # start Loop here
# # Make list and data frame to fill with data
# flamedata_list<-list()
# flamedata_list2<-flamedata_list
# pool_summary<-as.data.frame(matrix(nrow=length(dam_name), ncol=21))
# names(pool_summary)<-(c("Pool", "RiverKM_start", "RiverKM_end","Pool_length", "NO3_start", "NO3_end", "dNO3", "Turb_start", "Turb_end", "dTurb", "RNO3", "RTurb", "Q", "SPC_start", "SPC_end", "dSPC", "RSPC", 'Q_up', 'Q_down', 'Q_diff', 'Date'))
# pool_summary$Date<-as.Date(pool_summary$Date)
# 
# dam_nu<-1
# for (dam_nu in 1:length(dam_km)){
#   if (dam_nu==1){
#     sub<-NO3data[NO3data$riverkm<dam_km[dam_nu],]
#     sub2<-Turbdata[Turbdata$riverkm<dam_km[dam_nu],]
#     sub3<-SPCdata[SPCdata$riverkm<dam_km[dam_nu],]
#   } else {
#     sub<-NO3data[NO3data$riverkm<dam_km[dam_nu] & NO3data$riverkm>dam_km[dam_nu-1], ]
#     sub2<-Turbdata[Turbdata$riverkm<dam_km[dam_nu] & Turbdata$riverkm>dam_km[dam_nu-1], ] 
#     sub3<-SPCdata[SPCdata$riverkm<dam_km[dam_nu] & SPCdata$riverkm>dam_km[dam_nu-1], ] 
#   }
#   flame_date<-ymd(median(as.Date(sub$ltime)))
#   pool_summary[dam_nu, 21]<-ymd(median(as.Date(sub$ltime)))
#   flamedata_list[[dam_nu]]<-sub
#   flamedata_list2[[dam_nu]]<-sub2
#   names(flamedata_list)[[dam_nu]]<-dam_name[dam_nu]
#   names(flamedata_list2)[[dam_nu]]<-dam_name[dam_nu]
#   
#   #Distance
#   pool_summary[dam_nu,1]<-dam_name[dam_nu]
#   pool_summary[dam_nu,2:3]<-range(sub$riverkm)
#   pool_summary[dam_nu,4]<-pool_summary[dam_nu,3] - pool_summary[dam_nu,2]
#   
#   MR_Q_out<-AugQDaily[AugQDaily$Date==flame_date,c(dam_name[dam_nu])]
#   if (dam_nu>1){
#   MR_Q_in_test<-AugQDaily[AugQDaily$Date==flame_date,c(dam_name[dam_nu-1])]
#   } else if (dam_nu==1) {
#     MR_Q_in_test<-MR_Q_out
#   }
#   
#   # If Triburary exists in pool
#   if (dam_nu %in% InputChemistry$poolInterval){
#     
#     #Miss River Metrics
#     MR_NO3in<-median(sub$NITRATEM[1:10], na.rm=T)
#     MR_Turbin<-median(sub2$TurbFNU[1:20], na.rm=T)
#     MR_SPCin<-median(sub3$SpCndÂµS[1:20], na.rm=T)
#     MR_Q_in<- pool_summary[dam_nu-1,13]
#     
#     #Use Water Chem table for Pool 8 metrics
#     #flame data are bad because we sampled this stretch 3 times over 2 days
#     if (dam_name[dam_nu]=='8'){
#       MR_NO3in<-0.78
#       MR_Turbin<-9.01
#       MR_SPCin<-426.8
#     }   
#     
#     #Tributary Metrics
#     Trib_NO3in<- InputChemistry$NITRATEMG[InputChemistry$poolInterval==dam_nu]
#     Trib_Turbin<- InputChemistry$TurbFNU[InputChemistry$poolInterval==dam_nu]
#     Trib_SPCin<- InputChemistry$SpCondµScm[InputChemistry$poolInterval==dam_nu]
#     Trib_Q<- InputChemistry$Q[InputChemistry$poolInterval==dam_nu]
#     
#     MR_Q_in_test<-(MR_Q_in+Trib_Q)
#       
#     #NO3 initial
#     pool_summary[dam_nu,5] <- ((Trib_NO3in*Trib_Q) + (MR_NO3in*MR_Q_in)) / (MR_Q_in+Trib_Q)
#     #Turb initial
#     pool_summary[dam_nu,8]<- ((Trib_Turbin*Trib_Q) + (MR_Turbin*MR_Q_in)) / (MR_Q_in+Trib_Q)
#     #SPC initial
#     pool_summary[dam_nu,14]<- ((Trib_SPCin*Trib_Q) + (MR_SPCin*MR_Q_in)) / (MR_Q_in+Trib_Q)
# 
#   }  else {
#     #NO3 initial
#     pool_summary[dam_nu,5]<-median(sub$NITRATEM[1:10], na.rm=T)
#     #Turb initial
#     pool_summary[dam_nu,8]<-median(sub2$TurbFNU[1:20], na.rm=T)
#     #SPC initial
#     pool_summary[dam_nu,14]<-median(sub3$SpCndÂµS[1:20], na.rm=T)
#     
#   }
#   
#   #NO3 final
#   pool_summary[dam_nu,6]<-median(sub$NITRATEM[(length(sub$NITRATEM)-9):length(sub$NITRATEM)], na.rm=T)
#   #Turb final
#   pool_summary[dam_nu,9]<-median(sub2$TurbFNU[(length(sub2$TurbFNU)-19):length(sub2$TurbFNU)], na.rm=T)
#   #SPC final
#   pool_summary[dam_nu,15]<-median(sub3$SpCndÂµS[(length(sub3$SpCndÂµS)-19):length(sub3$SpCndÂµS)], na.rm=T)
# 
#   if (dam_name[dam_nu]=='8'){
#     pool_summary[dam_nu,6]<-0.768
#     pool_summary[dam_nu,9]<-6.46
#     pool_summary[dam_nu,15]<-414.7 
#   } 
#   #NO3 change
#   pool_summary[dam_nu,7]<- pool_summary[dam_nu,5] - pool_summary[dam_nu,6]
#   #NO3 Retention (0-1)
#   pool_summary[dam_nu,11]<-pool_summary[dam_nu,7]/pool_summary[dam_nu,5]
#   
#   #Turb change
#   pool_summary[dam_nu,10]<- pool_summary[dam_nu,8] - pool_summary[dam_nu,9]
#   #Turb Retention (0-1)
#   pool_summary[dam_nu,12]<-pool_summary[dam_nu,10]/pool_summary[dam_nu,8]
#   
#   #SPC change 
#   pool_summary[dam_nu,16]<- pool_summary[dam_nu,14] - pool_summary[dam_nu,15]
#   #Turb Retention (0-1)
#   pool_summary[dam_nu,17]<-pool_summary[dam_nu,16]/pool_summary[dam_nu,14]
#   
#   #Discharge out (cms)
#   if (length(MR_Q_out)==1){
#     pool_summary[dam_nu,13]<-MR_Q_out
#     pool_summary[dam_nu,19]<-MR_Q_out
#     pool_summary[dam_nu,18]<-MR_Q_in_test}
#   
#   
#   print(dam_name[dam_nu])
# }
# 
# pool_summary$Q_diff=pool_summary$Q_down-pool_summary$Q_up
# pool_summary$Q_percent=pool_summary$Q_diff/pool_summary$Q_down
# 
# 
# pool_summary$Pool[pool_summary$Pool!='Pepin'& !is.na(pool_summary$Pool)]<-paste("p", pool_summary$Pool[pool_summary$Pool!='Pepin'& !is.na(pool_summary$Pool)], sep="")
# 
# 
# hist(pool_summary$RNO3, breaks=100)
# 
# pool_summary$EvapRate_mmPerDay<-Weather$E_mmPerDay[match(pool_summary$Date, Weather$Date)]
# pool_summary$PoolArea<-summary_df$TotalArea[match(pool_summary$Pool, summary_df$Pool)]
# 
# pool_summary$Q_Evap_m3PerS<-pool_summary$EvapRate_mmPerDay*pool_summary$PoolArea/86.4
# pool_summary$Q_Evap_Per<-pool_summary$Q_Evap_m3PerS/pool_summary$Q_down
# summary(pool_summary$Q_Evap_Per)
# pool_summary$Q_Evap_Per
# 
# pool_summary$Q_WWTP<-rep(11, nrow(pool_summary))
# pool_summary$Q_WWTP_Per<-pool_summary$Q_WWTP/pool_summary$Q_down
# 
# pool_summary$Q_UnsampleTrib<-rep(max_TribQ, nrow(pool_summary))
# pool_summary$Q_UnsampleTrib_Per<-pool_summary$Q_UnsampleTrib/pool_summary$Q_down
# 
# 
# GW_rate_mPerS<-511/35.3147/sum(pool_summary$PoolArea[pool_summary$Pool%in% c('p7', 'p8')])/10^6
# 
# pool_summary$Q_GW<-pool_summary$PoolArea*(GW_rate_mPerS)*10^6
# pool_summary$Q_GW_Per<-pool_summary$Q_GW/pool_summary$Q_down
# 
# print(pool_summary)

# water_summary<-pool_summary[c('Pool', 'Q_up', 'Q_down', 'Q_diff', 'Q_percent', 'Q_Evap_m3PerS',  'Q_WWTP',  'Q_UnsampleTrib', 'Q_Evap_Per', 'Q_WWTP_Per', 'Q_UnsampleTrib_Per')]
# 
# print(water_summary)
# 
# water_summary$MissingWater_Total<-rowSums(abs(water_summary[c('Q_diff', 'Q_Evap_m3PerS', 'Q_WWTP', 'Q_UnsampleTrib')]))
# water_summary$MissingWater_Percent<-water_summary$MissingWater_Total/water_summary$Q_down
#   
# 
# setwd("E:/Git_Repo/nitrogen-retention")
# saveRDS(pool_summary, file = "UMR_R_dNO3_estimates.rds")
# saveRDS(water_summary, file='UMR_water_budget.rds')
# 

# #############################################
# New retention model including reviews
# #############################################



# start Loop here
# Make list and data frame to fill with data
flamedata_list<-list()
flamedata_list2<-flamedata_list
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
  names(flamedata_list)[[dam_nu]]<-dam_name[dam_nu]
  names(flamedata_list2)[[dam_nu]]<-dam_name[dam_nu]
  
  #Distance
  pool_summary2$Pool[dam_nu]<-dam_name[dam_nu]
  pool_summary2$RiverKM_start[dam_nu]<-min(sub$riverkm, na.rm=T)
  pool_summary2$RiverKM_end[dam_nu]<-max(sub$riverkm, na.rm=T)
  pool_summary2$Pool_length[dam_nu]<-pool_summary2$RiverKM_end[dam_nu] - pool_summary2$RiverKM_start[dam_nu]
  
  #Discharge out (cms)
  MR_Q_out<-AugQDaily[AugQDaily$Date==flame_date,c(dam_name[dam_nu])]
  MR_Q_out_3d<-Q_3DayAvg[Q_3DayAvg$Date==flame_date,c(dam_name[dam_nu])]
  
  if (length(MR_Q_out)==1){
    pool_summary2$Q_MRout[dam_nu]<-MR_Q_out
    pool_summary3DayAvg$Q_MRout[dam_nu]<-MR_Q_out_3d}

  #Discharge in (cms)
  if (dam_nu==1) {
    MR_Q_in<-PoolSAF$Flow_cms[match(flame_date, PoolSAF$Date)]
    MR_Q_in_3d<-mean(PoolSAF$Flow_cms[(match(flame_date, PoolSAF$Date)-2):match(flame_date, PoolSAF$Date)], na.rm=T)
  } else {
    MR_Q_in<-AugQDaily[AugQDaily$Date==flame_date,c(dam_name[dam_nu-1])]
    MR_Q_in_3d<-Q_3DayAvg[Q_3DayAvg$Date==flame_date,c(dam_name[dam_nu-1])]
  }
  pool_summary2$Q_MRin[dam_nu]<- MR_Q_in
  pool_summary3DayAvg$Q_MRin[dam_nu]<- MR_Q_in_3d

  #Miss River Concentrations
  MR_NO3in<-median(sub$NITRATEM[1:10], na.rm=T)
  MR_Turbin<-median(sub2$TurbFNU[1:20], na.rm=T)
  MR_SPCin<-median(sub3$SpCndÂµS[1:20], na.rm=T)
  
  #Use Water Chem table for Pool 8 metrics
  #flame data are bad because we sampled this stretch 3 times over 2 days
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
    Trib_SPCin<- InputChemistry$SpCondµScm[InputChemistry$poolInterval==dam_nu]
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
  pool_summary2$SPC_end[dam_nu]<-median(sub3$SpCndÂµS[(length(sub3$SpCndÂµS)-19):length(sub3$SpCndÂµS)], na.rm=T)
  pool_summary3DayAvg$SPC_end[dam_nu]<-median(sub3$SpCndÂµS[(length(sub3$SpCndÂµS)-19):length(sub3$SpCndÂµS)], na.rm=T)
  
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

# Calculate difference and retention using vector match
# This assumes that input discharge is conservative

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


pool_summary2$Pool[pool_summary2$Pool!='Pepin'& !is.na(pool_summary2$Pool)]<-
  paste("p", pool_summary2$Pool[pool_summary2$Pool!='Pepin'& !is.na(pool_summary2$Pool)], sep="")


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


pool_summary3DayAvg$Pool[pool_summary3DayAvg$Pool!='Pepin'& !is.na(pool_summary3DayAvg$Pool)]<-
  paste("p", pool_summary3DayAvg$Pool[pool_summary3DayAvg$Pool!='Pepin'& !is.na(pool_summary3DayAvg$Pool)], sep="")



#Other water sources

pool_summary2$EvapRate_mmPerDay<-Weather$E_mmPerDay[match(pool_summary2$Date, Weather$Date)]
pool_summary2$PoolArea<-summary_df$TotalArea[match(pool_summary2$Pool, summary_df$Pool)]

pool_summary2$Q_Evap_m3PerS<-pool_summary2$EvapRate_mmPerDay*pool_summary2$PoolArea/86.4
pool_summary2$Q_Evap_Per<-pool_summary2$Q_Evap_m3PerS/pool_summary2$Q_Totalin

pool_summary2$Q_WWTP<-rep(11, nrow(pool_summary2))
pool_summary2$Q_WWTP_Per<-pool_summary2$Q_WWTP/pool_summary2$Q_Totalin

pool_summary2$Q_UnsampleTrib<-rep(max_TribQ, nrow(pool_summary2))
pool_summary2$Q_UnsampleTrib_Per<-pool_summary2$Q_UnsampleTrib/pool_summary2$Q_Totalin


GW_rate_mPerS<-511/35.3147/sum(pool_summary2$PoolArea[pool_summary2$Pool%in% c('p7', 'p8')])/10^6

pool_summary2$Q_GW<-pool_summary2$PoolArea*(GW_rate_mPerS)*10^6
pool_summary2$Q_GW_Per<-pool_summary2$Q_GW/pool_summary2$Q_Totalin

delS<-abs(pool_summary2$Q_diff)
Potential<-rowSums(pool_summary2[c('Q_UnsampleTrib', 'Q_GW', 'Q_WWTP')]) - pool_summary2$Q_Evap_m3PerS
A<-rowMaxs(as.matrix(data.frame(delS, Potential)))

pool_summary2$PercentErrorWater<- A/pool_summary2$Q_MRout

print(pool_summary2)

water_summary<-pool_summary2[c('Pool', 'Q_MRin', 'Q_MRout', 'Q_Trib', 'Q_diff', 'Q_percent', 'Q_UnsampleTrib', 'Q_WWTP', 'Q_GW', 'Q_Evap_m3PerS', 'PercentErrorWater', 'Q_GW_Per', 'Q_Evap_Per', 'Q_WWTP_Per', 'Q_UnsampleTrib_Per', 'RiverKM_start', 'RiverKM_end')]

print(water_summary)



setwd("E:/Git_Repo/nitrogen-retention")
write.table(water_summary, file='Data/PoolWaterBudgets.csv', row.names=F, sep=',')
saveRDS(water_summary, file='Data/PoolWaterBudgets.rds')

# write.table(pool_summary2, file='Data/PoolSummary_Table S1.csv', row.names=F, sep=',')



#Uncertainty
#NO3 change
NO3max<-6.37
AvgQDiff<-(-1)*mean(pool_summary2$Q_diff[which(pool_summary2$Pool != 'Pepin')])
ExtraQ<-rowSums(abs(pool_summary2[c('Q_diff', 'Q_WWTP', 'Q_UnsampleTrib')]))

#Error bars using high amount of extra water (GW + Trib + WWTP)
HighN<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + ExtraQ*NO3max)/ (ExtraQ+pool_summary2$Q_Totalin-pool_summary2$Q_Evap_m3PerS)

LowN<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + ExtraQ*0)/ (ExtraQ+pool_summary2$Q_Totalin )

pool_summary2$dNO3_high<- (HighN - pool_summary2$NO3_end)
pool_summary2$dNO3_low<- (LowN - pool_summary2$NO3_end)

pool_summary2$RNO3_high<-pool_summary2$dNO3_high/HighN
pool_summary2$RNO3_low<-pool_summary2$dNO3_low/LowN

#Error bars using missing water as error
HighN_storage<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + abs(pool_summary2$Q_diff*NO3max))/ (abs(pool_summary2$Q_diff)+pool_summary2$Q_Totalin)

LowN_storage<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + abs(pool_summary2$Q_diff*0))/ (abs(pool_summary2$Q_diff)+pool_summary2$Q_Totalin)

pool_summary2$dNO3_high_storage<- HighN_storage - pool_summary2$NO3_end
pool_summary2$dNO3_low_storage<- LowN_storage - pool_summary2$NO3_end

pool_summary2$RNO3_high_storage<-pool_summary2$dNO3_high_storage/HighN_storage
pool_summary2$RNO3_low_storage<-pool_summary2$dNO3_low_storage/LowN_storage

#Error bars using average missing water as error
HighN_qdiff<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + AvgQDiff*NO3max)/ (AvgQDiff+pool_summary2$Q_Totalin)

LowN_qdiff<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + AvgQDiff*0)/ (AvgQDiff+pool_summary2$Q_Totalin)

pool_summary2$dNO3_high_qdiff<- HighN_qdiff - pool_summary2$NO3_end
pool_summary2$dNO3_low_qdiff<- LowN_qdiff - pool_summary2$NO3_end

pool_summary2$RNO3_high_qdiff<-pool_summary2$dNO3_high_qdiff/HighN_qdiff
pool_summary2$RNO3_low_qdiff<-pool_summary2$dNO3_low_qdiff/LowN_qdiff

#Average missing water or actual missing
HighN_option<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))*NO3max)/ (rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))+pool_summary2$Q_Totalin)

LowN_option<-(pool_summary2$NO3_start*pool_summary2$Q_Totalin + rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))*0)/ (rowMaxs(as.matrix(data.frame((pool_summary2$Q_diff*(-1)), rep(AvgQDiff, nrow(pool_summary2)))), dim=c(nrow(pool_summary2), 2))+pool_summary2$Q_Totalin)

pool_summary2$dNO3_high_option<- HighN_option - pool_summary2$NO3_end
pool_summary2$dNO3_low_option<- LowN_option - pool_summary2$NO3_end

pool_summary2$RNO3_high_option<-pool_summary2$dNO3_high_option/HighN_option
pool_summary2$RNO3_low_option<-pool_summary2$dNO3_low_option/LowN_option




# END new retention model

RNO3<-pool_summary2$RNO3
RNO3[(20:25)]<-NA

RSPC<-pool_summary2$RSPC
RSPC[(20:25)]<-NA

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





pool_summary2$dNO3_high_option




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


# ==============================
# Step 5 
# Merge tables and output single file
# bathy_df - Volume
# summary_df - Area and BW/I percentages
# pool_summary - Q, R, location
# ==============================

bathy_df$Pool
summary_df$Pool
pool_summary2$Pool
intersect(pool_summary2$Pool, intersect(bathy_df$Pool,summary_df$Pool))

merge1<-merge(summary_df, bathy_df, by='Pool', all=T)
merge2<-merge(merge1, pool_summary2, by='Pool', all=T)
merge2<-merge2[order(merge2$RiverKM_start),]

merge2<-merge2[which(merge2$Pool!=''),]
#Summarize Pool Areas


AllPools<-merge2[1,]
AllPools[1,]<-NA
AllPools$Pool<-"All Pools"
AllPools$TotalArea<-sum(merge2$TotalArea, na.rm=T)
AllPools$I_Area<-sum(merge2$TotalArea*merge2$I_Area, na.rm=T)/sum(merge2$TotalArea, na.rm=T)
AllPools$BWc_Area<-sum(merge2$TotalArea*merge2$BWc_Area, na.rm=T)/sum(merge2$TotalArea, na.rm=T)
# AllPools$RTurb<-(-0.833)

AllPools$NO3_start<-
  sum(c(InputChemistry$Q[1:11])*c(InputChemistry$NITRATEMG[1:11]))/
  sum(c(InputChemistry$Q[1:11]))
AllPools$NO3_end<-merge2$NO3_end[merge2$Pool=='p26']

  AllPools$dNO3<-AllPools$NO3_start-AllPools$NO3_end
AllPools$RNO3<-AllPools$dNO3/AllPools$NO3_start

AllPools$Turb_start<-
  sum(c(InputChemistry$Q[1:11])*c(InputChemistry$TurbFNU[1:11]))/
  sum(c(InputChemistry$Q[1:11]))
AllPools$Turb_end<-merge2$Turb_end[merge2$Pool=='p26']

AllPools$dTurb<-(AllPools$Turb_start-AllPools$Turb_end)
AllPools$RTurb<-AllPools$dTurb/AllPools$Turb_start

AllPools$SPC_start<-
  sum(c(InputChemistry$Q[1:11])*c(InputChemistry$SpCondµScm[1:11]))/
  sum(c(InputChemistry$Q[1:11]))
AllPools$SPC_end<-merge2$SPC_end[merge2$Pool=='p26']

AllPools$dSPC<-(AllPools$SPC_start-AllPools$SPC_end)
AllPools$RSPC<-AllPools$dSPC/AllPools$SPC_start


AllPools$Q_Totalin<-merge2$Q_Totalin[merge2$Pool=='p26']

merge3<-rbind(merge2, AllPools)

merge3$WRT_d<-merge3$Volume/merge3$Q_Totalin*(1000000/3600/24) #days
merge3$Z_mean_m<-merge3$Volume/merge3$TotalArea
merge3$H<-merge3$Q_Totalin/merge3$TotalArea*31.536

merge3$dNload<-merge3$dNO3*merge3$Q_Totalin


# merge3$Vf<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-merge3$RNO3))
# merge3$vf50<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-0.5))
# merge3$vf20<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-0.2))
# merge3$vf10<-((-1)*merge3$Z_mean_m/merge3$WRT_d*365 * log(1-0.1))

# merge3$Vf<-((-1)*merge3$H * log(1-merge3$RNO3))
# merge3$vf50<-((-1)*merge3$H * log(1-0.5))
# merge3$vf20<-((-1)*merge3$H * log(1-0.2))
# merge3$vf10<-((-1)*merge3$H * log(1-0.1))


#Uptake Rate mg N per m2 per day
# merge3$U_basedonVf<-merge3$Vf*merge3$NO3_start*1000/365

merge3$U<-merge3$dNO3*merge3$Q_Totalin/merge3$TotalArea*86.4

merge3$Vf<-merge3$U/merge3$NO3_start*365/1000


U2<-merge3$U
U2[(20:25)]<-NA
U2<-U2[1:(length(U2)-1)]


# merge3$U2<-merge3$dNO3*merge3$Q_Totalin/merge3$TotalArea*86.4
merge3$UNO3_high_option<-merge3$dNO3_high_option*merge3$Q_Totalin/merge3$TotalArea*86.4
merge3$UNO3_low_option<-merge3$dNO3_low_option*merge3$Q_Totalin/merge3$TotalArea*86.4



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

#Lake pepin percent of total removal
LoadRemoved<-merge3$dNO3*merge3$Q_Totalin
PepinPercentofTotalLoadRemoved<-LoadRemoved[which(merge3$Pool=='Pepin')]/LoadRemoved[which(merge3$Pool=='All Pools')]
Pool2PercentofTotalLoadRemoved<-LoadRemoved[which(merge3$Pool=='p2')]/LoadRemoved[which(merge3$Pool=='All Pools')]
Pool3PercentofTotalLoadRemoved<-LoadRemoved[which(merge3$Pool=='p3')]/LoadRemoved[which(merge3$Pool=='All Pools')]
print(PepinPercentofTotalLoadRemoved)
print(Pool2PercentofTotalLoadRemoved)
print(Pool3PercentofTotalLoadRemoved)


print(LoadRemoved[which(merge3$Pool=='All Pools')])

merge3$NO3_start*merge3$Q_Totalin

# names(merge3)[names(merge3) == 'Q'] <- 'Q_cms'

setwd('E:/Dropbox/FLAME_MississippiRiver')
write.table(merge3, "UMR_Pool_Summary_Table.csv", sep=",", row.names=F, col.names=T)

setwd("E:/Git_Repo/nitrogen-retention")
saveRDS(merge3, file = "UMR_Pool_Summary_Table.rds")



