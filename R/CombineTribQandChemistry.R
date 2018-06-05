
# Code combines Triburary discharge, water chemistry, and location

trib_df <- readRDS(file = "Outputs/UMRTribs_Q_DailyAug2015.rds")


# rm(list = ls())
options(digits=4)

# #Project Direcotry
# dir<-"E:/Git_Repo/MissRiver_Nitrogen"
# locdir<-"E:/Dropbox/ArcGIS"
# flamedir<-'E:/Dropbox/FLAME_MississippiRiver/Data/2015_UMR_AllDays'


#Get Tributary locations
tribs<-read.csv(paste(locdir, '/TribsAlongRoute.csv', sep=''), sep=",", header=TRUE, stringsAsFactors = F)
tribs$riverkm<-tribs$MEAS/1000
tribs<-tribs[order(tribs$MEAS, decreasing=FALSE),]

tribs$name2<-c("MN", "SC", "Ch", "Bl", "Rt", "WI", "Rk", "IA", "DM", "IL", "MO", "OH")
tribs2<-tribs[tribs$NAME!="Black River",]
tribs_add<-tribs2[1,]
tribs_add[1,]<-NA
tribs_add[1,c('riverkm')]<-c(491)
tribs_add[1, c('NAME', 'name2')]<-c("Maquoketa River", "MA")
tribs2<-rbind(tribs2, tribs_add)

tribs2<-tribs2[order(tribs2$MEAS, decreasing=FALSE),]



TribChemistry<-read.csv(paste(flamedir, '/UMR2015_AllWaterChemSamples.csv', sep=''), header=T, stringsAsFactors = F)

TribChemistry2<-  TribChemistry[TribChemistry$Sample.Notes %in%  c(unique(trib_df$name), 'UofM-Bridge') ,]
TribChemistry2$DateTime<-as.Date(TribChemistry2$DateTime, format="%m/%d/%Y")
TribChemistry2$Sample.Notes[which(TribChemistry2$Sample.Notes=='UofM-Bridge')]<-'Mississippi River'

TribChemistry2$Q<-NA
TribChemistry2$Q_2dayAvg<-NA
TribChemistry2$Q_3dayAvg<-NA

sample=1
for (sample in 1:nrow(TribChemistry2)){
  site<-TribChemistry2$Sample.Notes[sample]
  date<-TribChemistry2$DateTime[sample]
  table<-trib_df[trib_df$name==site,]
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

names(InputChemistry)[(grep("SpCond", names(InputChemistry)))]<-c('SPCuScm', 'SPCScm_t')

saveRDS(InputChemistry, file = "Outputs/UMR_TribuaryChemistryAndQ.rds")
write.table(InputChemistry, file = "Outputs/UMR_TribuaryChemistryAndQ.csv", row.names=F, sep=',')


