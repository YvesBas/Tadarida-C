library(data.table)
library(seewave)
library(tuneR)
#library(warbleR)

find_modes<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] > x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}


ListDir=c("C:/Users/Yves Bas/Documents/RSDB_HF/20200526","C:/Users/Yves Bas/Documents/RSDB_HF/20200527","C:/Users/Yves Bas/Documents/RSDB_HF/20200529"
          ,"C:/Users/Yves Bas/Documents/RSDB_HF/20200605","C:/Users/Yves Bas/Documents/RSDB_HF/20200702","C:/Users/Yves Bas/Documents/RSDB_HF/20200709"
          ,"C:/Users/Yves Bas/Documents/RSDB_HF/20200715")
#FFTlist=c(256,512,1024,2048)
BWmin=30
SpAlt=c("Glapoe","Glavar","Chanig","Scovir","Sconig","Scoleu","Scodin"
        ,"Mimmon","Mopcon","Chapum")


ListFiles=vector()
ListTA=vector()
ListETI=vector()
for (i in 1:length(ListDir))
{
  ListFiles=c(ListFiles,list.files(ListDir[i],pattern=".wav$",full.names=T))
  ListTA=c(ListTA,list.files(ListDir[i],pattern=".ta$",full.names=T
                             ,recursive=T))
  ListETI=c(ListETI,list.files(ListDir[i],pattern=".eti$",full.names=T
                               ,recursive=T))
}

DataTA=list()
for (g in 1:length(ListTA))
{
  DataTA[[g]]=fread(ListTA[g])
}
FeaturesTA=rbindlist(DataTA)

DataETI=list()
for (g in 1:length(ListETI))
{
  DataETI[[g]]=fread(ListETI[g])
  names(DataETI[[g]])=c(names(DataETI[[g]])[2:length(names(DataETI[[g]]))],"empty")
  DataETI[[g]]$Filename=gsub(".eti",".wav",basename(ListETI[g]))
}
ETI=rbindlist(DataETI)


ETI=subset(ETI,ETI$Espece!="")
ETITA=merge(ETI,FeaturesTA,by.y=c("Filename","CallNum")
            ,by.x=c("Filename","Cri"),all.x=T)
ETITAe=subset(ETITA,ETITA$Type=="echolo")
test=subset(ETITAe,ETITAe$Commentaire=="")
table(test$Espece,test$Indice)
ETITAe=subset(ETITAe,ETITAe$Commentaire!="")

ETITAe$SNR=(ETITAe$Amp1+ETITAe$Amp2)-(ETITAe$NoisePrev+ETITAe$NoiseUp+ETITAe$NoiseNext
                                      +ETITAe$NoiseDown)
ETITAe$SNRscaled=scale(log(ETITAe$SNR-13))


boxplot(ETITAe$FreqMP~ETITAe$Espece,las=2)

ListInd=unique(ETITAe$Commentaire)

for (i in 1:length(ListInd))
{
  ETi=subset(ETITAe,ETITAe$Commentaire==ListInd[i])
  if(quantile(ETi$PrevMP2,0.9)>20)
  {
    ETi=subset(ETi,ETi$PrevMP2>20)
  }
  
  
  print(table(ETi$Espece))
  if(nrow(ETi)>1)
  {
Dflow=density(ETi$CO2_FIF)
Dflow$y=Dflow$y*(1-c(0:511)/4096) 
plot(Dflow)
  modesFC=density(ETi$CO2_FIF)$x[find_modes(Dflow$y)]
  mainmode=density(ETi$CO2_FIF)$x[which.max(Dflow$y)]
  mainmode2=which(modesFC==mainmode)
  
  
  if(mainmode2>1)
  {
    ModeInf=ETi$CO2_FIF>(modesFC[mainmode2-1]+modesFC[mainmode2])/2
  }else{
    ModeInf=rep(T,nrow(ETi))
  }
  
  if(mainmode2<length(modesFC))
  {
    ModeSup=ETi$CO2_FIF<(modesFC[mainmode2+1]+modesFC[mainmode2])/2
  }else{
    ModeSup=rep(T,nrow(ETi))
  }
  DataMainMode=subset(ETi,ModeInf&ModeSup)
  }else{
    DataMainMode=ETi
  }
  
  if(nrow(DataMainMode)>1)
  {
  if(median(DataMainMode$CO2_Slope)<=0)
  {
    DataMainSlope=subset(DataMainMode
                         ,DataMainMode$CO2_Slope<=0)
  }else{
    DataMainSlope=subset(DataMainMode
                         ,DataMainMode$CO2_Slope>=0)
  }
  plot(DataMainSlope$CO2_Dur,DataMainSlope$CO2_Slope,ylim=c(-50,0))
  }else{
    DataMainSlope=DataMainMode
  }
  
  #ordonnancement selon pente et durée
  DataOrder=DataMainSlope[order(DataMainSlope$CO2_Slope,DataMainSlope$CO2_Dur)]
  NumCalls=min(6,nrow(DataOrder))
  QuantilesRef=c(0:(NumCalls-1))/(NumCalls-1)
  SlopeRef=quantile(abs(DataOrder$CO2_Slope),QuantilesRef,type=1)
  SlopeRef=SlopeRef[order(SlopeRef,decreasing=T)]
  SlopeRefScaled=1/(abs(SlopeRef)+0.02)
  DataOrder$SlopeScaled=(1/(abs(DataOrder$CO2_Slope)+0.02))
  
  BestCalls=data.frame()
  Fmin=vector()
  Fmax=vector()
  for (j in 1:NumCalls)
  {
    if(j>1)
    {
      Dataj=subset(DataOrder
                   ,DataOrder$SlopeScaled>(SlopeRefScaled[j-1]+SlopeRefScaled[j])/2)
    }else{
      Dataj=DataOrder
    }            
    
    if(j<NumCalls)
    {
      Dataj=subset(Dataj
                   ,Dataj$SlopeScaled<=(SlopeRefScaled[j+1]+SlopeRefScaled[j])/2)
    }
    print(nrow(Dataj))
    BestCallj=Dataj[which.max(Dataj$SNRscaled),]
    #print(BestCallj$StTime)
    #print(paste(BestCallj$Dur,BestCallj$CO2_Dur))
    FileJ=match(BestCallj$Filename,basename(ListFiles))
    BestCallj$FilenameFull=ListFiles[FileJ]
    
    #sauver puis concaténer le wave
    BestCalls=rbind(BestCalls,BestCallj)
    #print(nrow(BestCalls))
    Fstart=pmax(BestCallj$Fmin-0.2*BestCallj$BW,8)
    Fend=pmin(BestCallj$Fmax+0.2*BestCallj$BW,BestCallj$SampleRate/2000)
    #test=spectro(WaveJ,flim=c(Fstart,Fend)
    #            ,wl=256,ovlp=0.9,zp=256)
    
    Fmin=c(Fmin,Fstart)
    Fmax=c(Fmax,Fend)
    
  }
  if((BestCalls$Espece[1] %in% SpAlt)&(NumCalls==6))
  {
    BestCalls=BestCalls[c(1,4,2,5,3,6),]
  }
  
  DurW=median(BestCalls$CO2_Dur)
  DurW=max(DurW,10)
  
  Tstart=round((BestCalls$StTime/1000-DurW/1000/2)*
                 BestCalls$SampleRate)
  Tend=round((BestCalls$StTime/1000+DurW/1000*2.5)*
               BestCalls$SampleRate)
  
  WaveTot=NULL
    for (k in 1:nrow(BestCalls))
  {
    WaveJ=readWave(BestCalls$FilenameFull[k],from=Tstart[k],to=Tend[k])
    #print(c(Tstart,Tend))
    #print(summary(WaveJ))
    WaveJ@samp.rate=WaveJ@samp.rate*10
    if(is.null(WaveTot))
    {
      WaveTot=WaveJ
    }else{
      WaveTot=bind(WaveTot,WaveJ)
    }
  }
  
  
  savewav(WaveTot,f=WaveTot@samp.rate,paste0("./mnhn/Benin/ForSonograms/",ListInd[i]
                                             ,"_",BestCallj$Espece,".wav"))  
  
  FileImage=paste0("./mnhn/Benin/ForSonograms/",ListInd[i]
                   ,"_",BestCallj$Espece,".png")
  
  BW=max(Fmax)-min(Fmin)
  if((BW)<BWmin)
  {
    Fmaxs=max(Fmax)+(BWmin-BW)/2
    Fmins=min(Fmin)-(BWmin-BW)/2
    if(Fmaxs>WaveTot@samp.rate/1000/2)
    {
      Fmins=Fmins-(Fmaxs-WaveTot@samp.rate/1000/2)
    Fmaxs=WaveTot@samp.rate/1000/2
    }
    if(Fmins<0)
    {
      Fmaxs=Fmaxs-Fmins
      Fmins=0
    }
    
  }else{
        Fmaxs=max(Fmax)
        Fmins=min(Fmin)
      }
  
  test=spectro(WaveTot,flim=c(Fmins,Fmaxs)
               ,wl=256,ovlp=0.9,zp=128,collevels=c(-30:0)*2,plot=F)
  AmpMax=max(test$amp)
  AmpMin=min(test$amp)
  
  
  png(FileImage,width=960,height=480)
  
  test=spectro(WaveTot,flim=c(Fmins,Fmaxs)
               ,wl=256,ovlp=0.9,zp=128
               ,collevels=c(-30:0)*(1.3*AmpMax-AmpMin)/30/2+AmpMax)
  dev.off()
  
  
  #ggspectro(test,f=WaveJ@samp.rate)
  
  
  #test=spectro(WaveJ,tlim=c(4.15,4.21),flim=c(20,30),plot=F)
  #BestCallj$sound.files=BestCallj$Filename
  #BestCallj$start=BestCallj$StTime/1000-BestCallj$Dur/1000*2
  #BestCallj$end=BestCallj$StTime/1000+BestCallj$Dur/1000*3
  #BestCallj$selec=1
  #BCS=selection_table(BestCall)
  #setwd(dirname(ListFiles[FileJ]))
  #test=specreator(BestCall,flim=c(20,30))
  # plot(test)
}


