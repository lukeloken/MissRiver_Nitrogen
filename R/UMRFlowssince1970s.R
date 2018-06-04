


#Inputs for data retrevial 
parameterCd <- c("00060") # Discharge, NO3
startDate <- "1900-01-01"
endDate <- "2017-12-31"

# Get USGS gauge data for all UMR stations.
siteNumbers<-c("05288500",# Mississippi River
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

dischargeUnit$month<-month(dischargeUnit$Date)
dischargeUnit$year<-year(dischargeUnit$Date)

discharge1970<-dischargeUnit[dischargeUnit$year>=1970,]

StLouis<-dischargeUnit[which(dischargeUnit$site_no=='07010000'),]


site_no<-siteNumbers[9]
GaugeLocations<-c('Brooklyn Park, MN', 'St. Paul, MN', 'Hastings, MN', 'Prescot, WI', 'Winona, MN', 'Clinton, IA', 'Keokuk, IA', 'Grafton, IL', 'St. Louis, MO', 'Chester IL', 'Thebes, IL')

siteNumbers<-c("05288500",# Mississippi River (brooklyn park)
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
               '365730089063001') # Below Cairo

png("Figures/Discharge100YearRecord.png", res=600, width=6,height=12, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(tck=-0.02)
par(ps=10)
par(mar=c(2.5,2.5,1,1))
par(oma=c(1,1,0,0))
par(mfrow=c(6,2))
par(mgp=c(3,.3,0))

for (site_no in siteNumbers[1:11]){

boxplot(Flow_cms~month, data=dischargeUnit[which(dischargeUnit$site_no==site_no & dischargeUnit$Flow>=0),], col='darkgrey', cex=0.5, outline=F)
                                        
boxplot(Flow_cms~month, data=dischargeUnit[which(dischargeUnit$site_no==site_no & dischargeUnit$Flow>=0 & dischargeUnit$year=='2015'),], add=T, col='lightpink',boxwex=0.5, pch=16, outline=F, border='red')
mtext(site_no, 3, 0)

if(site_no == siteNumbers[1]){
  legend('topright', inset=0.01, c('70-100 year record', '2015'), pch=22, col=c('black', 'red'), pt.bg=c('darkgrey', 'lightpink'), bty='n', pt.cex=2)
}
print(site_no)
print(summary(dischargeUnit[which(dischargeUnit$site_no==site_no),]))

}

mtext(expression(paste('Discharge (m'^'3', ' s^', '-1', ')')), 2, -0.5, outer=T ,cex=1.5)
mtext('Month', 1, -0.5, outer=T, cex=1.5)


dev.off()



png("Figures/Discharge40YearRecord.png", res=600, width=9,height=11, units="in")

cex=1
par(cex=cex, cex.axis=cex)
par(tck=-0.02)
par(ps=10)
par(mar=c(2,2,1,.5))
par(oma=c(1.5,2,0.5,0))
par(mfrow=c(4,3))
par(mgp=c(3,.3,0))

for (site_no in siteNumbers[1:11]){
  
  boxplot(Flow_cms~month, data=discharge1970[which(discharge1970$site_no==site_no & discharge1970$Flow>=0),], col='darkgrey', cex=0.5, outline=F)
  
  boxplot(Flow_cms~month, data=discharge1970[which(discharge1970$site_no==site_no & discharge1970$Flow>=0 & discharge1970$year=='2015'),], add=T, col='lightpink',boxwex=0.5, pch=16, outline=F, border='red')
  mtext(site_no, 3, 0)
  
  if(site_no == siteNumbers[1]){
    legend('topleft', inset=0.01, c('Since 1970', '2015'), pch=22, col=c('black', 'red'), pt.bg=c('darkgrey', 'lightpink'), bty='n', pt.cex=2)
  }
  legend('topright', inset=0.01, GaugeLocations[which(site_no==siteNumbers)], bty='n')
  
  print(site_no)
  # print(summary(discharge1970[which(discharge1970$site_no==site_no),]))
  
}

mtext(expression(paste('Discharge (m'^'3', ' s'^'-1', ')')), 2, -0.5, outer=T ,cex=1.5)
mtext('Month', 1, 0, outer=T, cex=1.5)


dev.off()

