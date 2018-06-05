

library(dataRetrieval)
library(plyr)
library(tidyr)
library(dplyr)

#Trib names and dam number (note, not dam name)
GoodTribNames<-c("Minnesota River", "Saint Croix River", "Chippewa River", "Root River", "Wisconsin River", "Maquoketa River", "Rock River", "Iowa River", "Des Moines River", "Illinois River")  
PoolNu<-c(2, 3, 4, 9, 11, 14, 16, 18, 20, 25)

InputChemistry <- readRDS(file = "Outputs/UMR_TribuaryChemistryAndQ.rds")

pool_summary2<- readRDS(file='Outputs/UMR_RetentionEstimates_with_Uncertainty.rds')

#MissRiver Discharage at dams
DamQDaily <- readRDS(file = "Outputs/UMR_Q_Dams_Daily2015.rds")
summary(DamQDaily)

#Inputs for data retrevial 
parameterCd <- c("00060") # Discharge, NO3
startDate <- "2015-01-01"
endDate <- "2015-12-31"

# Get USGS gauge data for all UMR stations.
siteNumbers<-c('05288500', # Above minneapolis*** (pools 1)
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

UMR_Q<-spread(dischargeUnit[,c('site_no', 'Date', 'Flow_cms')], key=site_no, value=Flow_cms)


Pool1<-UMR_Q[c('Date', '05288500')]
Pool2<-UMR_Q[c('Date', '05331580')]
Pool3<-UMR_Q[c('Date', '05344500')]
Pool5A<-UMR_Q[c('Date', '05378500')]
Pool13<-UMR_Q[c('Date', '05420500')]
Pool19<-UMR_Q[c('Date', '05474500')]
Pool26<-UMR_Q[c('Date', '05587450')]
StLouis<-UMR_Q[c('Date', '07010000')]
Thebes<-UMR_Q[c('Date','07022000')]

# ################################
# Get Trib discharge from devtools
# ################################

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
                "05595000",#Kaskaskia River
                "03612600"#Ohio River
)


TribNames<-c('Mississippi River',
             'Minnesota River',
             'Saint Croix River',
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
             'Kaskaskia River', 
             'Ohio River'
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


trib_df <- ldply(trib_list, data.frame)[,1:7]
trib_widedf<-spread(trib_df[,c('.id', 'Date', 'Flow_cms')], key=.id, value=Flow_cms)

trib_shortdf<-trib_widedf[c('Date', "Minnesota River", "Saint Croix River", "Chippewa River", "Root River", "Wisconsin River", "Maquoketa River", "Rock River", "Iowa River Confluence", "Des Moines River", "Illinois River")  ]

summary(trib_shortdf)
names(trib_shortdf)<-c('Date', GoodTribNames)


#Join Dam and trib data
Q2015a<-right_join(DamQDaily, trib_shortdf)

Q2015<-full_join(Q2015a, Pool19)
# Q2015$DAM1<-Pool1$`05331000`

names(Q2015)<-c(names(Q2015)[1:(ncol(Q2015)-1)], 'DAM19')
Q2015<-Q2015[,c(1:19, 36, 20:35)]
head(Q2015)

QPercent<-Q2015[,c(1, 2:26)]
QPercent[,2:26]<-NA
Qcombined<-QPercent

PercentSummary<-data.frame(Dam=names(QPercent)[2:26], Qin=NA, Qout=NA, Qtrib=NA, Qcombined=NA, meanPercentDiff=NA, n=NA)

png("Figures/Pool_Discharge_Comparison2015.png", res=600, width=10,height=10, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(tck=-0.02)
par(ps=8)

par(mar=c(2.5,2.5,.25,.25))
par(oma=c(1.5,1.5,0,0))
par(mfrow=c(5,5))
par(mgp=c(2,.3,0))
Trib<-1
Dam_nu<-2
for (Dam_nu in 2:which(names(Q2015)=='DAM26')){
  
  x<-Q2015[,Dam_nu]
  
  if(Dam_nu==2){
    y<-Pool1$`05288500`
  } else {
  y<-Q2015[,Dam_nu-1]}
  
  z<-rep(0, length(x))
  
  lim=range(c(x , y), na.rm=T)
  plot(x,y, xlab='', ylab='', xlim=lim, ylim=lim)
  
  mtext(names(Q2015)[Dam_nu], 1, 1.25)
  if (Dam_nu == 2){
  mtext('USGS Gauge 05288500', 2, 1.25)
    } else { mtext(names(Q2015)[Dam_nu-1], 2, 1.25)}

  QPercent[,Dam_nu]<-(x-y)/x
  Qcombined[,Dam_nu]<-y
  # If Trib exists add river discharge

  if ((Dam_nu-1) %in% PoolNu){
    
    z<-Q2015[,which(names(Q2015)==GoodTribNames[Trib])]
  
    points(x, y+z, col='blue', pch=3)
    
    legend('topleft', inset=0.0, pch=3, paste(GoodTribNames[Trib]), bty='n', lty=0, cex=1.2, col='blue', text.col='blue')

    QPercent[,Dam_nu]<-(x-y-z)/x
    Qcombined[,Dam_nu]<-y+z
    
    Trib<-Trib+1
  }
  
  # plot 1:1 line
  abline(0,1, col='red', lwd=2)
  
  fullobs<-which(!is.na(x) & !is.na(y) & !is.na(z))
  
  PercentSummary$Qin[Dam_nu-1]<- mean(y[fullobs], na.rm=T)
  PercentSummary$Qout[Dam_nu-1]<- mean(x[fullobs], na.rm=T)
  PercentSummary$Qtrib[Dam_nu-1]<- mean(z[fullobs], na.rm=T)
  PercentSummary$Qcombined[Dam_nu-1]<- mean(z[fullobs]+y[fullobs], na.rm=T)
  PercentSummary$meanPercentDiff[Dam_nu-1] <- mean(((x-y-z)/x)[fullobs], na.rm=T)
  PercentSummary$n[Dam_nu-1]<-length(fullobs)
}

mtext(expression(paste('Incoming Q (m'^'3', ' s'^'-1', ')')), 2, -0.25, outer=T ,cex=1.5)
mtext(expression(paste('Outgoing Q (m'^'3', ' s'^'-1', ')')), 1, 0.5, outer=T, cex=1.5)

dev.off()

PercentSummary$Dam<-gsub('DAM', '', PercentSummary$Dam)
names(PercentSummary)[1]<-c('Pool')
print(PercentSummary)

write.table(PercentSummary, file='Outputs/2015DamFlowComparison.csv', row.names=F, sep=',')
saveRDS(PercentSummary, 'Outputs/2015DamFlowComparison.rds')


#Create Table S1 for paper

tribs_forS1<-trib_widedf[c('Date', "Mississippi River", "Minnesota River", "Saint Croix River", "Chippewa River", "Root River", "Wisconsin River", "Maquoketa River", "Rock River", "Iowa River Confluence", "Des Moines River", "Illinois River", 'Missouri River', 'Ohio River')  ]

MeanQ_2015<-colMeans(tribs_forS1[,-1], na.rm=T)

tableS1<-data.frame(River=names(MeanQ_2015), MeanQ=MeanQ_2015, row.names=NULL)
tableS1$River<-gsub(' Confluence', '', tableS1$River)

tableS1$PoolEntry<-c(1,2,3,4,8,10,13,16,18,20,26,'OR', 'OR')

tableS1$SampleDate<-InputChemistry$DateTime[match( tableS1$River, InputChemistry$Sample.Notes)]

tableS1$Q_SampleDate<-InputChemistry$Q[match(tableS1$River, InputChemistry$Sample.Notes)]

tableS1$NO3<-InputChemistry$NITRATEMG[match( tableS1$River, InputChemistry$Sample.Notes)]
tableS1$Turb<-InputChemistry$TurbFNU[match( tableS1$River, InputChemistry$Sample.Notes)]

tableS1$SPC<-InputChemistry$SPCuScm[match( tableS1$River, InputChemistry$Sample.Notes)]


tableS1$MRin<-pool_summary2$Q_MRin[match(tableS1$PoolEntry, gsub('p', '', pool_summary2$Pool))]

# tableS1$MRin[tableS1$River=='Missouri River']<-StLouis$`07010000`[which(StLouis$Date==tableS1$SampleDate[which(tableS1$River=='Missouri River')])]- tableS1$Q_SampleDate[which(tableS1$River=='Missouri River')]

tableS1$MRin[tableS1$River=='Missouri River']<-Pool26$'05587450'[which(Pool26$Date==tableS1$SampleDate[which(tableS1$River=='Missouri River')])]

tableS1$MRin[tableS1$River=='Ohio River']<-Thebes$`07022000`[which(Thebes$Date==tableS1$SampleDate[which(tableS1$River=='Ohio River')])]

tableS1$Q_Percent<-NA

tableS1$Q_Percent <- (tableS1$Q_SampleDate/(tableS1$MRin+tableS1$Q_SampleDate))

tableS1$Q_Percent[which(tableS1$River =='Mississippi River')]<-1

tableS1_out<-tableS1[c('River', 'PoolEntry', 'MeanQ', 'SampleDate', 'Q_SampleDate', 'Q_Percent', 'NO3', 'Turb', 'SPC')]

tableS1_out$River<-gsub(' River', '', tableS1_out$River)

saveRDS(tableS1_out, file = "Outputs/UMR_Tribuary_SummaryTable.rds")
write.table(tableS1_out, file = "Outputs/UMR_Tribuary_SummaryTable.csv", row.names=F, sep=',')

