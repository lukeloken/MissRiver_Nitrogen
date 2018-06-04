
#Model Expected Concentration across distance

InputChemistry <- readRDS(file = "Outputs/UMR_TribuaryChemistryAndQ.rds")
names(InputChemistry)[(grep("SpCond", names(InputChemistry)))]<-c('SPCuScm', 'SPCScm_t')

Missourikm<-InputChemistry$riverkm[InputChemistry$Sample.Notes=='Missouri River']

df<-InputChemistry[c('Sample.Notes', 'NITRATEMG', 'TurbFNU', 'SPCuScm', 'Q', 'Q_3dayAvg',  'Q_2dayAvg', 'riverkm')]
df$riverkm[which(df$Sample.Notes=='Mississippi River')]<-0

df2<-df


df2$NO3_Expect<-NA
df2$Turb_Expect<-NA
df2$SPC_Expect<-NA

df2$NO3_Expect_3d<-NA
df2$Turb_Expect_3d<-NA
df2$SPC_Expect_3d<-NA

df2$Q_cumulative<-NA
# df2$Q_2d_cumulative<-NA
df2$Q_3d_cumulative<-NA

row=1
for (row in 1:nrow(df2)){
  if (row==1){
    df2$Q_cumulative[row]<-df2$Q[row]
    df2$Q_3d_cumulative[row]<-df2$Q_3dayAvg[row]
    df2$NO3_Expect[row]<-df2$NITRATEMG[row]
    df2$Turb_Expect[row]<-df2$TurbFNU[row]
    df2$SPC_Expect[row]<-df2$SPCuScm[row]
    df2$NO3_Expect_3d[row]<-df2$NITRATEMG[row]
    df2$Turb_Expect_3d[row]<-df2$TurbFNU[row]
    df2$SPC_Expect_3d[row]<-df2$SPCuScm[row]
  } else {
    short_df<-df2[1:row,]
    df2$Q_cumulative[row]<-sum(short_df$Q)
    df2$Q_3d_cumulative[row]<-sum(short_df$Q_3dayAvg)
    
    df2$NO3_Expect[row]<-sum(short_df$NITRATEMG*short_df$Q)/df2$Q_cumulative[row]
    df2$Turb_Expect[row]<-sum(short_df$TurbFNU*short_df$Q)/df2$Q_cumulative[row]
    df2$SPC_Expect[row]<-sum(short_df$SPCuScm*short_df$Q)/df2$Q_cumulative[row]
    
    df2$NO3_Expect_3d[row]<-sum(short_df$NITRATEMG*short_df$Q_3dayAvg)/df2$Q_3d_cumulative[row]
    df2$Turb_Expect_3d[row]<-sum(short_df$TurbFNU*short_df$Q_3dayAvg)/df2$Q_3d_cumulative[row]
    df2$SPC_Expect_3d[row]<-sum(short_df$SPCuScm*short_df$Q_3dayAvg)/df2$Q_3d_cumulative[row]
  }
}

saveRDS(df2, file = "Outputs/UMR_ExpectedNO3Turb.rds")


