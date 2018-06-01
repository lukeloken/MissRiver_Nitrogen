
# Code to summarize UMR pools geographic features
# Inputs include shapefiles of aquatic areas and bathymetry

# rm(list = ls())
options(digits=4)

#Project Direcotry
dir<-"E:/Git_Repo/MissRiver_Nitrogen"
shapedir<-"E:/Dropbox/FLAME/basemaps/shapefiles"

library(rgdal)
library(rgeos)
library(plyr)
library(dplyr)


# ================================
# Step 1 
# Code to calculate surface area, volume, for each of the Upper Mississippi River pools
# ================================

# Get Lake Pepin Shapefile
Pepinshape<-readOGR(shapedir, "LakePepin", stringsAsFactors = F)


dir1<-(paste(shapedir, "/USGS_AquaticAreas", sep=""))
dir1_files<-list.files(dir1)

#Figure out file names
dir1_dbf<-dir1_files[grep(".dbf", dir1_files)]
dir1_shape<-sub(".dbf", "", dir1_dbf)

#Make dataframe and vector to fill with information
summary_df<-as.data.frame(matrix(nrow=length(dir1_shape)+1, ncol=8))
names(summary_df)<-c("Pool", "TotalArea", "MC_Area", "SC_Area", "I_Area", "BWc_Area", "LP_Area", "BWi_Area" )
all_codes<-c()
all_desc<-c()

#loop through all pool shapefiles
file=1
for (file in 1:length(dir1_shape)){
i<-dir1_shape[file]

name_i<-sub("aqa_1989_", "", i)
name_i<-sub("_z15n83", "", name_i)
summary_df[file,1]<-name_i

shape_i<-readOGR(dir1, i, stringsAsFactors = F)
shape_i<-shape_i[!shape_i$AQUA_CODE %in% c("N", "NOPH"),]

shape_i$Area_Calc<-(sapply(shape_i@polygons, function(x) x@Polygons[[1]]@area))/1000000
all_codes<-c(all_codes, unique(shape_i$AQUA_CODE))
all_desc<-c(all_desc, unique(shape_i$AQUA_DESC))

summary_df[file, 2]<-sum(shape_i$Area_Calc)

shape_i_MC<-shape_i@data[shape_i$AQUA_CODE %in% c('MCB','MNC'), ]
shape_i_SC<-shape_i@data[shape_i$AQUA_CODE %in% c('SC', 'TC', 'TRC', 'EC'), ]
shape_i_I<-shape_i@data[shape_i$AQUA_CODE %in% c('CIMP'), ]
shape_i_LP<-shape_i@data[shape_i$AQUA_CODE %in% c('CTDL'), ]
shape_i_BWc<-shape_i@data[shape_i$AQUA_CODE %in% c('CACL', 'CFDL', 'CMML', 'CFSA', 'CBP', 'CLLL'), ]
shape_i_BWi<-shape_i@data[shape_i$AQUA_CODE %in% c('IACL', 'IFDL', 'IMML', 'IBP', 'ILLL', 'ITDL', 'IFSA', 'ISCL'), ]

if (name_i=="p04"){
  summary_df[file, 2]<-summary_df[file, 2]-sum(shape_i_LP$Area_Calc)
  summary_df[file, 7]<-sum(shape_i_LP$Area_Calc)/summary_df[file, 2]
  summary_df[file, 6]<-sum(shape_i_BWc$Area_Calc)/summary_df[file, 2]
  summary_df[nrow(summary_df), 1]<-"Pepin"
  summary_df[nrow(summary_df), 2]<-sum(shape_i_LP$Area_Calc)
} else {
  summary_df[file, 7]<-NA
  summary_df[file, 6]<-(sum(shape_i_LP$Area_Calc)+sum(shape_i_BWc$Area_Calc))/summary_df[file, 2]
}

summary_df[file, 3]<-sum(shape_i_MC$Area_Calc)/summary_df[file, 2]
summary_df[file, 4]<-sum(shape_i_SC$Area_Calc)/summary_df[file, 2]
summary_df[file, 5]<-sum(shape_i_I$Area_Calc)/summary_df[file, 2]

summary_df[file, 8]<-sum(shape_i_BWi$Area_Calc)/summary_df[file, 2]
print(summary_df[file,])

}
#Change pool names to match final merge and omit empty data
summary_df$Pool<-sub("5a", "5A", summary_df$Pool)
summary_df$Pool<-sub("p0", "p", summary_df$Pool)
summary_df<-summary_df[!is.na(summary_df$TotalArea),]
summary_df<-subset(summary_df, select=-LP_Area)
summary_df<-summary_df[-grep("or", summary_df$Pool),]

unique_codes<-unique(all_codes)
unique_desc<-unique(all_desc)
unique_table<-data.frame(unique_codes, unique_desc)

#Save outputs
setwd(dir)

write.table(summary_df, "Outputs/UMR_Pool_Areas.csv", sep=",", row.names=F, col.names=T)
saveRDS(summary_df, file = "Outputs/UMR_Pool_Areas.rds")


# ===========================================
# Step 2
# Calculate Volume for pools with Bathy data
# ===========================================

dir2<-(paste(shapedir, "/USGS_Bathy", sep=""))
dir2_files<-list.files(dir2)

#Figure out file names
dir2_dbf<-dir2_files[grep("z15n83.dbf", dir2_files)]
dir2_shape<-sub(".dbf", "", dir2_dbf)


#Make dataframe and vector to fill with information
bathy_df<-as.data.frame(matrix(nrow=length(dir2_shape)+1, ncol=2))
names(bathy_df)<-c("Pool", "Volume")
grid_codes<-c()
depths<-c()

bathy=1
#loop through all bathy shapefiles
for (bathy in 1:length(dir2_shape)){
  i<-dir2_shape[bathy]
  
  name_i<-sub("bath_", "", i)
  name_i<-sub("_z15n83", "", name_i)
  name_i<-strsplit(name_i, split="_", fixed = T)[[1]][2]
  bathy_df[bathy,1]<-name_i

  shape_i<-readOGR(dir2, i, stringsAsFactors = F)
  shape_i<-shape_i[!shape_i$GRID_CODE %in% c(9999,-9999),]
  shape_i<-shape_i[!is.na((shape_i$DEPTH_M_)),]
  shape_i$GRID_CODE<-as.numeric(shape_i$GRID_CODE)
  
  grid_codes<-c(grid_codes, unique(shape_i$GRID_CODE))
  depths<-c(depths, unique(shape_i$DEPTH_M_)) 
  
  l<-strsplit(shape_i$DEPTH_M_, split=" ", fixed = T)
  df<-rbind.fill(lapply(l,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
  df <- data.frame(sapply(df, function(x) as.numeric(as.character(x))))
  
  #Calculate mean depth for each polygon. Input shapefile displays depth as a range (e.g., "0.2 - 0.4"). Code selects for middle of range. 
  df$V4<-NA
  row=2
  for (row in 1:nrow(df)){
    if (is.finite(df$V2[row])){
      df[row,4]<-df$V2[row]}
    else {
    df[row,4]<-mean(unlist(df[row,c(1,3)]))
    }
  }
  
  shape_i$AREA<-(sapply(shape_i@polygons, function(x) x@Polygons[[1]]@area))/1000000
  shape_i$Volume<-NA
  shape_i$Depth_Max_<-NA
  shape_i$Depth_Mean<-df[,4]
  # shape_i$DoverSA<-NA
  
  shape_i$Depth_Max_<-shape_i$GRID_CODE/100
  shape_i$Volume<-shape_i$Depth_Mean*shape_i$AREA

  bathy_df[bathy,2]<-sum(shape_i$Volume)
  
  # For Pool 4, clip Lake Pepin and summarize it separately
  if (name_i=="p4"){
    
    #find polygons that fall within lake pepin and select those. 
    clip<-gIntersects(Pepinshape, shape_i, byid=T)
    clip2<-as.vector(clip)
    clipped<-shape_i[clip2,]
  
    bathy_df[nrow(bathy_df),2]<-sum(clipped$Volume)
    bathy_df[nrow(bathy_df),1]<-"Pepin"
    
    # Remove Pepin metrics from Pool 4 summary
    bathy_df[bathy,2]<-sum(shape_i$Volume)-sum(clipped$Volume)
  }
    
  print(bathy_df[bathy,])
}
bathy_df[,2]<-round(bathy_df[,2], digits=1)

print(bathy_df)


#Save outputs
setwd(dir)

write.table(bathy_df, "Outputs/UMR_Pool_Volumes.csv", sep=",", row.names=F, col.names=T)
saveRDS(bathy_df, file = "Outputs/UMR_Pool_Volumes.rds")


# #############################
# Step 3
# Combine Pool Areas and Volumes
# #############################

merge1<-dplyr::full_join(summary_df, bathy_df, by='Pool')
merge1<-merge1[which(merge1$Pool!=''),]

merge2<-merge1[c(1:3, nrow(merge1), 4:(nrow(merge1)-1)),]

merge2$Z_mean_m<-merge2$Volume/merge2$TotalArea

#Save outputs
setwd(dir)

write.table(merge2, "Outputs/UMR_Pool_AreasAndVolumes.csv", sep=",", row.names=F, col.names=T)
saveRDS(merge2, file = "Outputs/UMR_Pool_AreasAndVolumes.rds")


