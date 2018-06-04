# Script to call other scripts and process UMR data
rm(list = ls())

setwd("E:/Git_Repo/MissRiver_Nitrogen")

#Project Direcotries
dir<-"E:/Git_Repo/MissRiver_Nitrogen"
shapedir<-"E:/Dropbox/FLAME/basemaps/shapefiles"
locdir<-"E:/Dropbox/ArcGIS"
flamedir<-'E:/Dropbox/FLAME_MississippiRiver/Data/2015_UMR_AllDays'
datadir<-'E:/Dropbox/FLAME_MississippiRiver'

#Calculate UMR Pool areas and volume
source('R/SummarizeUMRPoolAreas.R')

# Using Dataretrieval download USGS gauge station discharge for UMR survey
source('R/DownloadConcurrentDischarge.R')

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

#Loop through UMR data folders (datadir). Collect flame data and merge with LTER samples
source('R/MergeWaterChemwithFlameSites.R')

source('R/PlotLTRM_NO3Timeseries.R')

source('R/DamDischargeComparison.R')
source('R/UMRFlowssince1970s.R')
