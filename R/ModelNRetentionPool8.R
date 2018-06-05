# ###################################################
# Script to calculate NO3 retention in Pool 8
# Code will run for all of the Pool 8 sampling events
# Inputs include discharge (Q) and [NO3] (mg N / L)
# Inputs include Dam 7, Root and Lacrosse Rivers
# Output is condition at Dam 8
# ###################################################

# load packages
library(dataRetrieval)
library(lubridate)
library(MASS)

# set start and end date
StartDate<-"2014-01-01"
EndDate<-"2016-11-03"

# setwd("E:/Dropbox/FLAME_MississippiRiver")

# Get Dam 7 Discharge Data
Dam7Q<-read.table('Data/ld7_Q_2014.txt', header=T, skip=5, stringsAsFactors = F, sep="")
Dam7Q$DATE<-as.Date(Dam7Q$DATE, format="%d%b%Y")
Dam7Q<-Dam7Q[!is.na(Dam7Q$DATE),]
Dam7Q$Flow_cms<-Dam7Q$INST.VAL/35.3147

# Get Dam 8 Discharge Data
Dam8Q<-read.table('Data/ld8_Q_2014.txt', header=T, skip=5, stringsAsFactors = F, sep="")
Dam8Q$DATE<-as.Date(Dam8Q$DATE, format="%d%b%Y")
Dam8Q<-Dam8Q[!is.na(Dam8Q$DATE),]
Dam8Q$Flow_cms<-Dam8Q$INST.VAL/35.3147

# ==================================
# Get FLAME and LTER Water Chem Data
# ==================================

AllMissMerged <- readRDS(file='Outputs/MissRiver_WaterChem_FlameLTER.rds')
WaterChemData <- AllMissMerged

# WaterChemData<-read.csv(paste(datadir, '/MissRiverFlameLabMerged.csv', sep=''), header=T, stringsAsFactors = F)
WaterChemData$Date<-as.Date(WaterChemData$DateTime, format="%Y-%m-%d")

WaterChemData$NO3AVG<-rowMeans(data.frame(WaterChemData$'NO3 NO2'/1000, WaterChemData$NITRATEMG), na.rm=T)

site_names<-c('D8', 'D7', 'Root', 'Black', 'Crosse')

dam8samples<-grep(site_names[1], WaterChemData$Sample.Notes)
dam7samples<-grep(site_names[2], WaterChemData$Sample.Notes)
rootsamples<-grep(site_names[3], WaterChemData$Sample.Notes)
blacksamples<-grep(site_names[4], WaterChemData$Sample.Notes)
lacrossesamples<-grep(site_names[5], WaterChemData$Sample.Notes)

SampleDates<-unique(WaterChemData$Date[c(dam8samples, dam7samples, rootsamples, blacksamples, lacrossesamples)])
SampleDates<-SampleDates[order(SampleDates)]

SampleChemData<-WaterChemData[WaterChemData$Date %in% SampleDates, ]
SampleChemData<-SampleChemData[order(SampleChemData$Date),]

#Figure out which days are part of the same survey
today=2
SampleChemData$group<-c(1, rep(NA, nrow(SampleChemData)-1))
for (today in 2:nrow(SampleChemData)){
  if (SampleChemData$Date[today] - SampleChemData$Date[today-1] <7){
    SampleChemData$group[today]<-SampleChemData$group[today-1]
    } else {
    SampleChemData$group[today]<-SampleChemData$group[today-1]+1
  }
}



dam8Data<-SampleChemData[grep(site_names[1], SampleChemData$Sample.Notes),]
dam7Data<-SampleChemData[grep(site_names[2], SampleChemData$Sample.Notes),]
rootData<-SampleChemData[grep(site_names[3], SampleChemData$Sample.Notes),]
blackData<-SampleChemData[grep(site_names[4], SampleChemData$Sample.Notes),]
lacrosseData<-SampleChemData[grep(site_names[5], SampleChemData$Sample.Notes),]


# ==================================
# Get Tributary Water Chemistry and Discharge Data
# ==================================

#Triburary River gauge data
TsiteNumbers<-c("05382000",#Black River Upper
                "05383075",#LaCrosse River
                "05385000",#Root River Main
                "05385500",#Root River South
                "05379500",#Trempeleau
                "05378500", # UMR Winona (LD 5a) 
                "05420500" # UMR Clinton (LD 13) 
)

TribNames<-c('Black River Upper',
             'LaCrosse River',
             'Root River Main',
             'Root River South',
             'Trempeleau',
             'UMR Winona (LD 5a)', 
             'UMR Clinton (LD 13)')

parameterCd <- c("00060") # Discharge

tribtable<-cbind(TribNames, TsiteNumbers)

trib_list<-list()
for (trib in 1:length(TsiteNumbers)){
  TDischarge<-readNWISdv(TsiteNumbers[trib], parameterCd, StartDate, EndDate)
  TDischarge <- renameNWISColumns(TDischarge)
  TDischarge$Flow_cms<-TDischarge$Flow /35.3147
  
  trib_list[[trib]]<-TDischarge
  names(trib_list)[trib]<-TribNames[trib]
}

Rootmerge<-merge(trib_list[c('Root River Main')][[1]], trib_list[c('Root River South')][[1]], by='Date', all=T)
Rootmerge$Flow_cms<-rowSums(data.frame(Rootmerge$Flow_cms.x, Rootmerge$Flow_cms.y), na.rm=T)
trib_list[[length(trib_list)+1]]<-Rootmerge
names(trib_list)[[length(trib_list)]]<-'Root River'

LD7merge1<-merge(trib_list[c('UMR Winona (LD 5a)')][[1]], trib_list[c('Trempeleau')][[1]], by='Date', all=T)
LD7merge2<-merge(LD7merge1, trib_list[c('Black River Upper')][[1]], by='Date', all=T)
LD7merge2$Flow_cms<-rowSums(data.frame(LD7merge2$Flow_cms.x, LD7merge2$Flow_cms.y, LD7merge2$Flow_cms), na.rm=T)
trib_list[[length(trib_list)+1]]<-LD7merge2
names(trib_list)[[length(trib_list)]]<-'UMR_ALD7'

# ==================================
# Loop through dates and make mass balance
# ==================================


# start Loop here
# Make list and data frame to fill with data
pool_summary<-as.data.frame(matrix(nrow=length(unique(SampleChemData$group))+2, ncol=10), stringsAsFactors = F)
names(pool_summary)<-(c("Date", "NO3_start", "NO3_end", "dNO3", "RNO3", "Dam7_Q", "Dam8_Q", "RR_Q", "USGS_Q", "Temp"))

row<-9
for (row in c(unique(SampleChemData$group), length(unique(SampleChemData$group))+1:2)){
  GroupChemData<-SampleChemData[SampleChemData$group==row,]
  day<-round_date(median(GroupChemData$Date), 'day')
  
  # set days for winter dates
  if (row==length(unique(SampleChemData$group))+1){day<-'2015-01-12'}
  if (row==length(unique(SampleChemData$group))+2){day<-'2016-01-14'}

  # Get NO3 values for ins and outs
  # Mississippi In, Mississippi out, Root, Black, and LaCrosse Rivers)
  
  MR_in<-mean(dam7Data$NO3AVG[dam7Data$group==row])
  MR_out<-mean(dam8Data$NO3AVG[dam8Data$group==row])
  RR_in<-mean(rootData$NO3AVG[rootData$group==row])
  BR_in<-mean(blackData$NO3AVG[blackData$group==row])
  LR_in<-mean(lacrosseData$NO3AVG[lacrosseData$group==row])
  
  # Use USGS NO3 data for one date. Samples analyzed in Boulder, CO
  if (day=='2014-10-02'){
    MR_in<-66.297*14.007/1000
    MR_out<-64.059*14.007/1000
  }
  
  #Jan 2015 use USGS NO3 data. Samples analyzed in Boulder, CO
  if (day=='2015-01-12'){
    MR_in<-120.15*14.007/1000
    MR_out<-124.85*14.007/1000
    LR_in<-188.16*14.007/1000
    BR_in<-104.34*14.007/1000
    RR_in<-344.92*14.007/1000
  }
  
  # Jan 2016 use USGS NO3 data. Samples analyzed in Boulder, CO
  if (day=='2016-01-14'){
    MR_in<-313.90*14.007/1000
    MR_out<-313.77*14.007/1000
    LR_in<-163.32*14.007/1000
    BR_in<-101.11*14.007/1000
    RR_in<-444.58*14.007/1000
  }
  
  
  #Substitue mean if NA
  if (is.na(LR_in)){
    LR_in<-mean(lacrosseData$NO3AVG, na.rm=T)}
  if (is.na(RR_in)){  
    RR_in<-mean(rootData$NO3AVG, na.rm=T)}
  if (is.na(BR_in)){  
    BR_in<-mean(blackData$NO3AVG, na.rm=T)}

  # Get Q for Tributaries
  RR_in_Q<-trib_list[[c("Root River")]][trib_list[[c("Root River")]]$Date==day,c('Flow_cms')]
  LR_in_Q<-trib_list[[c("LaCrosse River")]][trib_list[[c("LaCrosse River")]]$Date==day,c('Flow_cms')]
  
  # Set Black River to specific flow (1500 cfs in summer, 500 cfs in winter)
  if (month(day) %in% c(11,12,1,2,3)){
    BR_in_Q<-500/35.3147}
  if (!month(day) %in% c(11,12,1,2,3)){
    BR_in_Q<-1500/35.3147}

  
  # Get Q for Main Channel
  MR_in_Q<-Dam7Q$Flow_cms[Dam7Q$DATE==day]-BR_in_Q
  MR_out_Q<-Dam8Q$Flow_cms[Dam8Q$DATE==day]
  USGS_Q<-trib_list[[c("UMR_ALD7")]][trib_list[[c("UMR_ALD7")]]$Date==day,c('Flow_cms')]
  
  # NO3 mass balance
  In_NO3<-((MR_in*MR_in_Q) + (RR_in*RR_in_Q) + (LR_in*LR_in_Q) + (BR_in*BR_in_Q) ) / (MR_in_Q+RR_in_Q+LR_in_Q+BR_in_Q)
  Out_NO3<-MR_out
  delta_NO3<-In_NO3-Out_NO3
  R_NO3<-delta_NO3/In_NO3
  
  # export to summary table
  pool_summary[row,1]<-as.character(day)
  if (length(R_NO3)>0){
    pool_summary[row,2]<-In_NO3
    pool_summary[row,3]<-Out_NO3
    pool_summary[row,4]<-delta_NO3
    pool_summary[row,5]<-R_NO3
    pool_summary[row,6]<-MR_in_Q    
    pool_summary[row,7]<-MR_out_Q
    pool_summary[row,8]<-RR_in_Q
    pool_summary[row,9]<-USGS_Q
    pool_summary[row,10]<-mean(GroupChemData$TempC, na.rm=T)
  } else {print('Missing Data')}
}
pool_summary$Date<-as.Date(pool_summary$Date)
pool_summary$RR_pct<-pool_summary$RR_Q/pool_summary$Dam8_Q 
pool_summary[length(unique(SampleChemData$group))+1,10]<-1
pool_summary[length(unique(SampleChemData$group))+2,10]<-1
pool_summary<-pool_summary[order(pool_summary$Date),]
print(pool_summary)

#ouputs with uncertainty estimates
write.table(pool_summary, file='Outputs/Pool8_RetentionEstimates.csv', row.names=F, sep=',')
saveRDS(pool_summary, file='Outputs/Pool8_RetentionEstimates.rds')

