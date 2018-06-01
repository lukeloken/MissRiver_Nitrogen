

#Select with Dam/USGS gauge to use for each pool 

#load flow data
DamQDaily <- readRDS( file = "Outputs/UMR_Q_Dams_Daily2015.rds")
dischargeUnit <- readRDS(file = "Outputs/UMR_Q_USGS_DailyAug2015.rds")

startDate <- "2015-07-30"
endDate <- "2015-08-31"


#Link USGS gauge stations with a flow at a specific dam (at the pool outlet!)
PoolSAF<-dischargeUnit[dischargeUnit$site_no=='05288500', c('Date', 'Flow_cms')]
Pool1<-data.frame(Date=dischargeUnit[dischargeUnit$site_no=='05331000', c('Date')], Flow_cms=dischargeUnit[dischargeUnit$site_no=='05331000', c('Flow_cms')]-trib_df$Flow_cms[trib_df$name=='Minnesota River'])

Pool2<-dischargeUnit[dischargeUnit$site_no=='05331580', c('Date', 'Flow_cms')]
05331580
Pool3<-dischargeUnit[dischargeUnit$site_no=='05344500', c('Date', 'Flow_cms')]
Pool5A<-dischargeUnit[dischargeUnit$site_no=='05378500', c('Date', 'Flow_cms')]
Pool13<-dischargeUnit[dischargeUnit$site_no=='05420500', c('Date', 'Flow_cms')]
Pool19<-dischargeUnit[dischargeUnit$site_no=='05474500', c('Date', 'Flow_cms')]
Pool26<-dischargeUnit[dischargeUnit$site_no=='05587450', c('Date', 'Flow_cms')]

#Create dataframes to populate with flows
AugQDaily<-subset(DamQDaily, Date>=startDate & Date<=endDate)
names(AugQDaily)<-sub("DAM", "", names(AugQDaily))


#For dams near USGS gauges replace Flow at the dam with USGS flow estimates
AugQDaily$'1' <-Pool1$Flow_cms[match(AugQDaily$Date, Pool1$Date)]
AugQDaily$'2' <-Pool2$Flow_cms[match(AugQDaily$Date, Pool2$Date)]
AugQDaily$'3' <-Pool3$Flow_cms[match(AugQDaily$Date, Pool3$Date)]
AugQDaily$'5A' <-Pool5A$Flow_cms[match(AugQDaily$Date, Pool5A$Date)]
AugQDaily$'13' <-Pool13$Flow_cms[match(AugQDaily$Date, Pool13$Date)]
AugQDaily$'19' <-Pool19$Flow_cms[match(AugQDaily$Date, Pool19$Date)]
AugQDaily$'26' <-Pool26$Flow_cms[match(AugQDaily$Date, Pool26$Date)]

#For Lake Pepin and Pool 15 use flows at dam 3 and 14 respectively
AugQDaily$Pepin<-AugQDaily$'3'
AugQDaily$'15'<-AugQDaily$'14'


#Create two other tables using rolling 2 and 3 day means

AugQDaily_2DayAvg<-data.frame(AugQDaily$Date, sapply(AugQDaily[,2:28], rollmean, k=2, align='right', fill=NA))
AugQDaily_3DayAvg<-data.frame(AugQDaily$Date, sapply(AugQDaily[,2:28], rollmean, k=3, align='right', fill=NA))

names(AugQDaily_2DayAvg)<-names(AugQDaily)
names(AugQDaily_3DayAvg)<-names(AugQDaily)


#output files
saveRDS(AugQDaily, file = "Outputs/UMR_Q_AugDailyDamsOrUSGS.rds")
saveRDS(AugQDaily_2DayAvg, file = "Outputs/UMR_Q_AugDaily_2DAVG_DamsOrUSGS.rds")
saveRDS(AugQDaily_3DayAvg, file = "Outputs/UMR_Q_AugDaily_3DAVG_DamsOrUSGS.rds")
write.table(AugQDaily, file = "Outputs/UMR_Q_AugDailyDamsOrUSGS.csv", row.names=F, sep=',')
write.table(AugQDaily_2DayAvg, file = "Outputs/UMR_Q_AugDaily_2DAVG_DamsOrUSGS.csv", row.names=F, sep=',')
write.table(AugQDaily_3DayAvg, file = "Outputs/UMR_Q_AugDaily_3DAVG_DamsOrUSGS.csv", row.names=F, sep=',')

