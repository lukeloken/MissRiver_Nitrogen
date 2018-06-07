# Script to call other scripts and process UMR data
rm(list = ls())

setwd("E:/Git_Repo/MissRiver_Nitrogen")

#Project Direcotries

shapedir<-"E:/Dropbox/FLAME/basemaps/shapefiles" # contains Pool shapefiles (bathy and area)
locdir<-"E:/Dropbox/ArcGIS" #contains linear referenced tables (data, tribs, dams)

datadir<-'E:/Dropbox/FLAME_MississippiRiver' #Contains most sample data
datadir_dup<-'E:/Dropbox/FLAME_MississippiRiver_Duplicate' #Contains sample data for UMR survey
# dir<-"E:/Git_Repo/MissRiver_Nitrogen"
# flamedir<-'E:/Dropbox/FLAME_MississippiRiver/Data/2015_UMR_AllDays'

#Calculate UMR Pool areas and volume
# source('R/SummarizeUMRPoolAreas.R')

# Using Dataretrieval download USGS gauge station discharge for UMR survey
source('R/DownloadConcurrentDischarge.R')

#Loop through UMR data folders (datadir). Collect flame data and merge with LTER samples
source('R/MergeWaterChemwithFlameSites.R')

# merge tributary chemistry with flow
source('R/CombineTribQandChemistry.R')

# Combine USACE and USGS gauge data and select either for dam flow
source('R/SelectUSGSorLDdischarge.R')

# Loop through dams and calcuate retention for August 2015 UMR survey
source('R/ModelNRetentionUMRPools.R')

# Using Dataretrieval download USGS gauge station discharge for UMR survey
source('R/PlotRetentionWithUncertainty.R')

#Clculate expected NO3, Turb, and SPC based on conservative mixing
source('R/CalculateExpectedNO3TurbSPC.R')

# Plot longitudinal profile with expected and N load
# Link discharge measurement with each flame measurement
source('R/PlotLongitudinalProfileNO3TurbSPC_withExpected.R')



#Load LTRM NO3 data, merge with LTER data and plot timeseries
source('R/PlotLTRM_NO3Timeseries.R')

#Compare flows at UMR dams using all of 2015 data. Export water budget and figure
source('R/DamDischargeComparison.R')

#Compare 2015 flows to historic conditions at USGS gauge stations along UMR. 
source('R/UMRFlowssince1970s.R')

#Calcualte retention for Pool 8 all dates
source('R/ModelNRetentionPool8.R')

#Plot NO3 retention in Pool 8
source('R/PlotRetentionPool8.R')

#Merge Nitrous Oxide data and plot three panel Lake Pepin figure
source('R/MergeN2OData.R')
