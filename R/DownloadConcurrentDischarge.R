
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

# output files
write.table(dischargeUnit, "Outputs/UMR_QDailyAug2015.csv", sep=",", row.names=F, col.names=T)
saveRDS(dischargeUnit, file = "Outputs/UMR_QDailyAug2015.rds")


# ==================================
# Step 3
# Get Tributary Discharge Data
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

#Merge two Root River sites
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

trib_df<-ldply(trib_list, data.frame)
trib_df<-trib_df[,1:7]
names(trib_df)[1]<-'name'

# output files
write.table(trib_df, "Outputs/UMR_TribsQDailyAug2015.csv", sep=",", row.names=F, col.names=T)
saveRDS(trib_df, file = "Outputs/UMR_TribsQDailyAug2015.rds")

