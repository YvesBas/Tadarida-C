library(data.table)
library(warbleR)
library(bioacoustics)
library(tools)
library(randomForest)

RSDB="./RSDB_HF"
FirstDir="20140801"
LastDir="20210113"
ETITA=fread("RSDB_HF_tabase3HF_sansfiltre.csv")

ListDir=list.dirs(RSDB,recursive=F)
FirstM=match(FirstDir,basename(ListDir))
LastM=match(LastDir,basename(ListDir))
ListDir=ListDir[FirstM:LastM]
#ListDir=c("./RSDB_HF/20200526","./RSDB_HF/20200527","./RSDB_HF/20200529"
 #         ,"./RSDB_HF/20200605","./RSDB_HF/20200702","./RSDB_HF/20200709"
  #        ,"./RSDB_HF/20200715")
FFTlist=c(256,512,1024,2048)

ListFiles=vector()
ListTA=vector()
ListETI=vector()
for (i in 1:length(ListDir))
{
  ListFiles=c(ListFiles,list.files(ListDir[i],pattern=".wav$",full.names=T))
  ListTA=c(ListTA,list.files(paste0(ListDir[i],"/txt"),pattern=".ta$",full.names=T
                             ,recursive=F))
  ListETI=c(ListETI,list.files(paste0(ListDir[i],"/eti"),pattern=".eti$",full.names=T
                               ,recursive=F))
}

CorrEti=gsub("/eti","",ListETI)
CorrEti=gsub(".eti",".wav",CorrEti)

test=match(ListFiles,CorrEti)
ListFiles=subset(ListFiles,!is.na(test))
CorrTa=gsub("/txt","",ListTA)
CorrTa=gsub(".ta",".wav",CorrTa)

test=match(ListFiles,CorrTa)
ListFiles=subset(ListFiles,!is.na(test))

ListFiles=subset(ListFiles,substr(basename(ListFiles),1,3)!="Cir")

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
boxplot(ETITAe$FreqMP~ETITAe$Espece,las=2)


Features=data.frame()
for (h in FFTlist)
{
  for (j in 1:length(ListFiles))
  {
    Wavej=readWave(ListFiles[j])
    Taj=fread(paste0(dirname(ListFiles[j]),"/txt/"
                     ,gsub(".wav",".ta",basename(ListFiles[j]))))
    TExp=Taj$SampleRate[1]/Wavej@samp.rate
    
    Featuresj=blob_detection(ListFiles[j]
                             ,time_exp=TExp,FFT_size=h,HPF=10000
                             ,min_dur = 0.5,max_dur=200)
    Featuresj=Featuresj$event_data
    Featuresj$Filename=basename(ListFiles[j])
    Featuresj$FFT_size=h
    if(is.data.frame(Featuresj))
    {
      Features=rbind(Features,Featuresj)
    }
    if(j%%10==1){print(paste(j,h))}
  }
}
plot(Features$duration,Features$temp_bandwith)
fwrite(Features,"Features.csv",sep=";")


FeaturesT=data.frame()
for (h in FFTlist)
{
  for (j in 1:length(ListFiles))
  {
    Wavej=readWave(ListFiles[j])
    Taj=fread(paste0(dirname(ListFiles[j]),"/txt/"
                     ,gsub(".wav",".ta",basename(ListFiles[j]))))
    if(nrow(Taj)>0)
    {
    TExp=Taj$SampleRate[1]/Wavej@samp.rate
    Featuresj=threshold_detection(ListFiles[j]
                                  ,time_exp=TExp,FFT_size=h,HPF=10000
                                  ,min_dur = 0.5,max_dur=200)
    Featuresj=Featuresj$event_data
    Featuresj$Filename=basename(ListFiles[j])
    Featuresj$FFT_size=h
    if(is.data.frame(Featuresj))
    {
      FeaturesT=rbind(FeaturesT,Featuresj)
    }
    if(j%%10==1){print(paste(j,h))}
  }
  }
}
fwrite(FeaturesT,"FeaturesT.csv",sep=";")


Features$starting_time=as.numeric(gsub("00:00:","",Features$starting_time))
FeaturesT$starting_time=as.numeric(gsub("00:00:","",FeaturesT$starting_time))
NvarBlob=ncol(Features)
NvarThreshold=ncol(FeaturesT)
Time=ETITAe$StTime+ETITAe$PosMP*ETITAe$Dur

FeaturesBioA=data.frame(matrix(nrow=nrow(ETITAe),ncol=(ncol(Features)+ncol(FeaturesT))*4))
for (z in 1:nrow(ETITAe))
{
  if(z%%1000==1){print(paste(z,Sys.time(),ETITAe$Espece[z]))}
  FileZ=ETITAe$Filename[z]
  TimeZ=Time[z]
  FreqZ=ETITAe$FreqMP[z]
  FZ=subset(Features,Features$filename==FileZ)
  #FZ$starting_time=as.numeric(gsub("00:00:","",FZ$starting_time))
  FZT=subset(FZ,(FZ$starting_time*1000<=TimeZ)&(FZ$starting_time*1000+FZ$duration>=TimeZ))
  FZTF=subset(FZT,(FZT$quant_2.5*0.00119<=FreqZ+1)&(FZT$quant_97.5*0.00119>=FreqZ-1))
  compteur=0
  for (g in FFTlist)
  {
    Fg=subset(FZTF,FZTF$FFT_size==g)
    if(nrow(Fg)>0)
    {
      Fg=Fg[order(Fg$duration,decreasing=T),]      
      FeaturesBioA[z,(compteur*NvarBlob)+c(1:ncol(Fg))]=Fg
    }
    compteur=compteur+1
  }
  
  TZ=subset(FeaturesT,FeaturesT$filename==FileZ)
  #FZ$starting_time=as.numeric(gsub("00:00:","",FZ$starting_time))
  TZT=subset(TZ,(TZ$starting_time*1000<=TimeZ)&(TZ$starting_time*1000+TZ$duration>=TimeZ))
  TZTF=subset(TZT,(TZT$freq_min*0.001<=FreqZ+1)&(TZT$freq_max*0.001>=FreqZ-1))
  compteur=0
  for (g in FFTlist)
  {
    Fg=subset(TZTF,TZTF$FFT_size==g)
    if(nrow(Fg)>0)
    {
      Fg=Fg[order(Fg$duration,decreasing=T),]      
      FeaturesBioA[z,(NvarBlob*length(FFTlist))+(compteur*NvarThreshold)+c(1:ncol(Fg))]=Fg
    }
    compteur=compteur+1
  }
  
  
  
  
}

names(FeaturesBioA)=c(paste0(rep(names(Features),length(FFTlist)),"_B")
                      ,paste0(rep(names(FeaturesT),length(FFTlist)),"_T"))

FFTnames=vector()
for (x in 1:length(FFTlist))
{
  FFTnames=c(FFTnames,rep(FFTlist[x],length(names(Features))))
}
for(w in 1:length(FFTlist))
{
  FFTnames=c(FFTnames,rep(FFTlist[w],length(names(FeaturesT))))
}

names(FeaturesBioA)=paste0(names(FeaturesBioA),"_",FFTnames)

FeaturesAll=cbind(ETITAe,FeaturesBioA)

fwrite(FeaturesAll,"FeaturesAll.csv")


