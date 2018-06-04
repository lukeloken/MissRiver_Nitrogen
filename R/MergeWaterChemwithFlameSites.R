
# ###########################################################
# Code to download all FLAME and LTER lab data and merge
# Code looks in Dropbox/FlameMississippi River subdirectories
# Will only currently work on Loken Desktop
# ###########################################################

library(gtools)

# get LTER chemlab data
# This file needs to be placed in project directory/Data
# File must start with 'LTER_waterchem' and should contain the data of download
# File must also be a '.csv' and in the standard download format from chemlab

allfiles<-list.files('Data')
waterchemfile<-allfiles[grep('LTER_waterchem', allfiles, ignore.case=T)]

LTERdata<-do.call("rbind", lapply(paste('Data/', waterchemfile, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE, stringsAsFactors = F))


# sample tests (e.g., DOC, NO3)
# sample ids (LTER barcodes)
tests<-unique(LTERdata$Test)
samplesID<-unique(LTERdata$Sample.ID)

# Convert long datatable into a wide table
chemtable<-data.frame(matrix(ncol=1, nrow=length(samplesID)))
names(chemtable)[1]<-c('Sample.ID')
chemtable$Sample.ID<-samplesID

for (test in tests){
  testtable<-subset(LTERdata,Test==test)
  smalltable<-testtable[,c(6,12)]
  names(smalltable)[2]<-test
  chemtable<-merge(chemtable, smalltable, by='Sample.ID', all.x=T)
}



# Get Mississippi River Data
# Look in Loken Desktop Dropbox/FLAME_MississippiRiver folder

directories<-list.files(paste(datadir, '/Data', sep=''))
# allcsv<-data.frame()
k<-1
for (k in 1:length(directories)){
  dir<-paste(datadir, '/Data/', directories[k], sep="")
  list<-list.files(paste(dir, sep=""))
  download<-list[grep('_Samples', list, ignore.case=T)]
  if(length(download)>=1){
    csv<-read.csv(paste(dir, download, sep="/"), header=T, stringsAsFactors = F)
    
    if (k==1){
      allcsv<-csv}
    if (k>1){
      allcsv<-smartbind(allcsv, csv, fill=NA)}
  }
}


#Mississippi River data were saved in Central Time
allcsv$DateTime<-as.POSIXct(allcsv$DateTime, format= "%Y-%m-%d %H:%M:%S", tz="America/Chicago")

# ###################
# And merge with LTER
# ###################

AllMissMerged<- merge(allcsv, chemtable, by.x='Sample.Number', by.y='Sample.ID', all.x=T)

saveRDS(AllMissMerged, file='Outputs/MissRiver_WaterChem_FlameLTER.rds')
write.table(AllMissMerged, file='Outputs/MissRiver_WaterChem_FlameLTER.csv', sep=",", row.names=F)
#End Code to Merge FLAME and LTER Data. Put this in it's own script
