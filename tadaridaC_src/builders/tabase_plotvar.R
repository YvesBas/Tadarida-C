library(data.table)
tabase=fread("tabase3HF_sansfiltre.csv")
VarY="CM_FIF"
#Var2="CS_Slope"
VarX="Slope5dB"
ParLog="x"
IntY=c(15,50)
IntX=c(1e-1,10)


for (i in 1:nlevels(as.factor(tabase$Espece)))
{
  Sp=levels(as.factor(tabase$Espece))[i]

tabaseSp=subset(tabase,tabase$Espece==Sp)
tabaseSp$AmpTot=tabaseSp$Amp1+tabaseSp$Amp2+tabaseSp$Amp3+tabaseSp$Amp4
tabaseSp$Slope5dB=(tabaseSp$CM_5dBAF-tabaseSp$CM_5dBBF)/tabaseSp$CM_5dBDur

#tabaseSp$Slope_B_FlF=(tabaseSp$CM_FIF-tabaseSp$StF)/(tabaseSp$)

col1=match(VarY,names(tabaseSp))
col2=match(VarX,names(tabaseSp))
tabaseSp=as.data.frame(tabaseSp)
MedInterval=quantile(tabaseSp$PrevMP2,0.5)
print(MedInterval)
Echo=(tabaseSp$PrevMP2<MedInterval/2)
tabase_SE=subset(tabaseSp,!Echo)
tabase_SH=subset(tabase_SE,tabase_SE$Ramp_1_2<0.2)
tabase_SH=subset(tabase_SH,tabase_SH$Ramp_2_3<0.2)

#plot(tabase_SE[,col1],tabase_SE$Hlo_AmpDif,ylim=c(-1000,1000))
#plot(tabase_SE[,col1],tabase_SE$HeiRM,ylim=c(0,0.1))




#plot(tabase_SH[,col1],tabase_SH$AmpTot,ylim=c(0,100))

tabase_Good=subset(tabase_SH,(tabase_SH$AmpTot>120)&(tabase_SH$HeiRM>0.05))

#for (h in 11:ncol(tabase_Good))
#  for (h in 151:280)
    
 # {
#  print(plot(tabase_Good[,col1],tabase_Good[,h],main=names(tabase_Good)[h]))
#}
        




Sel=(tabase_Good$CM_FIF>40)
subset(tabase_Good$Filename,Sel)
subset(tabase_Good$CallNum,Sel)
subset(tabase_Good$StTime,Sel)
subset(tabase_Good$CM_FIF,Sel)
subset(tabase_Good$AmpTot,Sel)
subset(tabase_Good$HeiRM,Sel)
tabase_Good=tabase_Good[order(tabase_Good[,col2]),]


if(grepl("x",ParLog)){
  if(mean(tabase_Good[,col2])<0)
  {
    if(i==1){
    plot(-tabase_Good[,col2],tabase_Good[,col1],log=ParLog,xlab=VarX
         ,ylab=VarY,ylim=IntY,xlim=IntX, col=i)
    }else{
      points(-tabase_Good[,col2],tabase_Good[,col1],col=i)
      
    }
    loessMod70 <- loess(tabase_Good[,col1] ~ tabase_Good[,col2], data=tabase_Good, span=0.70) # 50% smoothing span
    smoothed70 <- predict(loessMod70) 
    lines(smoothed70, x=-tabase_Good[,col2], col=i,lwd=2)
    
}
  }else{
    plot(tabase_Good[,col2],tabase_Good[,col1],log=ParLog,xlab=VarX
         ,ylab=VarY,ylim=IntY,xlim=IntX, col=i)
    loessMod70 <- loess(tabase_Good[,col1] ~ tabase_Good[,col2], data=tabase_Good, span=0.70) # 50% smoothing span
    smoothed70 <- predict(loessMod70) 
    lines(smoothed70, x=tabase_Good[,col2], col=i,lwd=2)
    
}

}

