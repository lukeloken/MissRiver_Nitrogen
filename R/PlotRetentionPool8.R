

pool_summary<-readRDS(file='Outputs/Pool8_RetentionEstimates.rds')



#Time series of Pool 8 NO3 retention and potential drivers
png("Figures/N_retention_Drivers_Intrapool.png", res=200, width=4.2,height=4, units="in")
cex=0.8
cexpt=1.5
par(cex=cex)
par(ps=12)

cex=0.8
par(cex=cex)
cexpt=1.5
ps=12
par(mfrow=c(2,2))
par(mar=c(3,1,0.5,0.5), oma=c(0,2.5,0,0))
par(mgp=c(3,.5,0))
par(tck=-0.03)
par(pch=16)

plot(pool_summary$Date, pool_summary$RNO3*100, type="o", las=1, cex.axis=cex, cex=cexpt)
mtext("Date", 1, 2, cex=cex)
abline(h=0)

plot(pool_summary$Dam8_Q, pool_summary$RNO3*100, yaxt="n", cex.axis=cex, cex=cexpt)
mtext(expression(paste("Discharge (m"^"3", " s"^"-1", ")", sep="")), 1, 2, cex=cex)
axis(2, labels=NA)
abline(h=0)

plot(pool_summary$Temp, pool_summary$RNO3*100, cex.axis=cex, las=1, cex=cexpt)
mtext(expression(paste("Temperature (", degree, "C)", sep="")), 1, 2, cex=cex)
abline(h=0)

plot(pool_summary$NO3_start, pool_summary$RNO3*100, yaxt="n", cex.axis=cex, cex=cexpt)
mtext(expression(paste('Incoming ', NO[3], " (mg N L"^"-1", ")")),1,2, cex=cex)
axis(2, labels=NA)
abline(h=0)

mtext(expression(paste(NO[3], ' Retention (%)')),2,1, outer=T, cex=cex)

dev.off()


#Regression models

#Single Linear Models
# Q_model<-lm(pool_summary$RNO3~pool_summary$Dam8_Q)
# summary(Q_model)
# Temp_model<-lm(pool_summary$RNO3~pool_summary$Temp)
# summary(Temp_model)             
# NO3_model<-lm(pool_summary$RNO3~pool_summary$NO3_start)
# summary(NO3_model)

#Multiple Linear Models with stepwise selection (AIC - Both)
null_model<-lm(pool_summary$RNO3~1)
full_model<-lm(pool_summary$RNO3~pool_summary$Dam8_Q + pool_summary$Temp + pool_summary$NO3_start)
# summary(full_model)
# step_model<-step(null_model, scope=list(lower=null_model, upper=full_model), direction='both')
# anova(step_model)
# summary(step_model)

#Another Function but gives same result
step_model2<-stepAIC(full_model, direction='both')
anova(step_model2)
print(summary(step_model2))


