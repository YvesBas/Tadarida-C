options(error = function() traceback(2))

Var_AGarder=c("Filename","CallNum","Version","FileDur","SampleRate"
              ,"StTime","Dur","PrevSt","Fmax","Fmin","BW"
              ,"FreqMP","CM_FIF","Amp1","Amp2","Amp3","Amp4", 
              "Hup_AmpDif", "Hlo_AmpDif", "CO2_FIF", "FreqSup17",
              "Ramp_2_1","Ramp_3_2", "Ramp_1_2", "Ramp_4_3", "Ramp_2_3", # Ramp_3_1 not taken because artefact
              "RAN_2_1", "RAN_3_2", "RAN_1_2", "RAN_4_3", "RAN_2_3") # RAN_3_1 not taken because artefact

obslist=talistot # Lists of .ta files to classify
FIO=FITA # file.info(talistot)

skip=F

if(length(args)==0){
  print("usage: Rscript TadaridaC.r <directory>")
  q()
}

library(randomForest) #to use the classifier (4.6.14)
library(data.table) #to handle large numbers of .ta files (1.11.8)
library(MASS) # to handle reduction (lda predict) (7.3-50)
library(Hmisc) # to perform fuzzy matching


#get the .ta files list
if (length(obslist) == 0) {
  print("no .ta files to process")
  q()
}

start=max(1,as.numeric(args[8])) # Block start number
end=min(length(obslist),as.numeric(args[9])) # Block end number

# Read sound features (.ta files)
Sys.time()
my.data <- list()
# for(f in 1:length(obslist)) {
for(f in start:end) {   #0.026 sec/files
  if(FIO$size[f]>1000){
    my.data[[f]] <- read.table(obslist[[f]],sep="\t",h=T)
  }
}
Sys.time()

# load the classifier
if (exists("ClassifEspA")==FALSE) 
{
  if(substr(basename(obslist[1]),1,3)=="Cir")
  {
    load(args[15])
  }else{
    load(args[2])
  }
}
if (exists("ClassifEspA")==FALSE) ClassifEspA=ClassifEsp3 #temp for test

#concatenate all the features table
CTP=as.data.frame(rbindlist(my.data,use.names=T,fill=T))

# Add a binary feature to help sort out birds from bats (based on if FreqMP>17)
CTP$FreqSup17=ifelse(CTP$FreqMP>17, 1, 0)

# #discard DSE suspected to be a harmonic
# #For lower harmonics
# if(is.na(match("Hlo_AmpDif",names(CTP)))==F)
# {
# CTP=subset(CTP,CTP$Hlo_AmpDif ==0)
# }
# #For upper harmonics
# if(is.na(match("Hup_AmpDif",names(CTP)))==F)
# {
# CTP=subset(CTP,CTP$Hup_AmpDif ==0)
# }

#discard DSE suspected to be from the same call as next DSE
#but separated due to low quality
CTP$PrevStMinDur=999
for (i in 2:nrow(CTP)){
  {
    CTP$PrevStMinDur[i]=CTP$PrevMP2[i]-CTP$Dur[i]*CTP$PosMP[i]-CTP$Dur[i-1]*(1-CTP$PosMP[i-1])
  }
}

CTP=subset(CTP, CTP$PrevStMinDur>5)


# Discard Harmonics
CTP_woH=subset(CTP,(CTP$Hup_AmpDif==0)&(CTP$Hlo_AmpDif==0)) # Subset the dataset without harmonics
CTP_H=subset(CTP,(CTP$Hup_AmpDif!=0)|(CTP$Hlo_AmpDif!=0)) # Subset the dataset with harmonics
hist(CTP_H$FreqMP,breaks=80)

CTP_Hsel=CTP_H[0,]
for (y in 1:length(unique(CTP_H$Filename)))
{
  CTPy=subset(CTP_H,CTP_H$Filename==unique(CTP_H$Filename)[[y]])
  #criterion
  if(nrow(CTPy)>1){
    DensityCriterion=density(CTPy$FreqMP,adjust=0.1)
    
    for (z in 1:nrow(CTPy))
    {
      CTP_Hz=subset(CTPy,CTPy$StTime<(CTPy$StTime[z]+CTPy$Dur[z])&
                      ((CTPy$StTime+CTPy$Dur)>CTPy$StTime[z])) # Subset simultaneous calls to call z = potential harmonics
      CTP_Hz=subset(CTP_Hz,CTP_Hz$CallNum!=CTPy$CallNum[z]) # exclude call z from this subset
      if(nrow(CTP_Hz)!=0){
        Critz=find.matches(CTPy$FreqMP[z],DensityCriterion$x,tol=1,maxmatch=1) # find closest match between peak frequency of call z and density of peak frequencies in file
        CritH=find.matches(CTP_Hz$FreqMP,DensityCriterion$x,tol=1,maxmatch=1) # find closest match between peak frequency of potential harmonics and density of peak frequencies in file
        Valz=DensityCriterion$y[Critz$matches[1]] # density value of density peak closest to FreqMP of call z
        ValH=DensityCriterion$y[CritH$matches] # density value of density peak closest to FreqMP of potential harmonics
        if(Valz>max(ValH)) # if density of z is superior to max density of potential harmonics
        {
          CTP_Hsel=rbind(CTP_Hsel,CTPy[z,]) # output call z because not a harmonic
        }}
    }}else{
      CTP_Hsel=rbind(CTP_Hsel,CTPy)
    }
}

CTP=rbind(CTP_woH,CTP_Hsel) # include calls that are not a harmonic it in dataset
CTP=CTP[order(CTP$Filename,CTP$StTime),]
# hist(CTP_Hsel$FreqMP)
# plot(CTP_Hsel$StTime,CTP_Hsel$FreqMP)

#discard DSE below 8 kHz or 0.8 kHz
if(is.na(match("FreqMP",names(CTP)))==F)
{
  CTP=subset(CTP,CTP$FreqMP>as.numeric(args[4]))
}

#discard false positives due to bin bleed on the last column
CTP=subset(CTP,CTP$StTime<(1000*CTP$FileDur-2))

CTP[is.na(CTP)]=0



CTP0=CTP


testReduc=grepl("LD",row.names(ClassifEspA$importance))
print(mean(testReduc))

if (mean(testReduc)>0.5) #when a reduction of variables is necessary (=low frequency case)
{
  
  #baseref pour recalculer les variables "rankées"
  if(exists("tabase3")==F){tabase3=fread(paste0(args[3],".csv"))}
  if(exists("Scaling")==F){Scaling=readRDS(paste0(args[3],"_Reduc1.rds"))}
  
  for (i in 4:274)
  {
    temp=CTP0[,i]
    test=as.vector(tabase3[,..i])
    test=test[order(test[,1]),]
    cuts=c(-Inf,as.vector(test [[1]]),Inf)
    test2=findInterval(temp,cuts,left.open=T)
    CTP[,i]=test2
  }
  
  
  Reduc2=predict(Scaling,CTP[,4:274])
  
  CTP0=cbind(CTP0,Reduc2$x)
}

# Prediction
if(nrow(CTP)>0)
{
  print("Before prediction")
  print(nrow(CTP))
  print(gc())
  #get the predictions and the main features (noticeably the file name)
  ProbEsp0 <- predict(ClassifEspA, CTP0,type="prob",norm.votes=TRUE)
  print(nrow(ProbEsp0))
  print(getwd())
  print("After prediction")
  print(gc())
  
  # something about features to keep for sorting/aggregating later
  Test_AG=match(Var_AGarder,colnames(CTP0))
  Col_AG=unique(Test_AG[!is.na(Test_AG)])
  print(length(Col_AG))
  CTP_AG=subset(CTP0,select=Col_AG)
  
  print(ncol(CTP_AG))
  
  #Loop init
  ProbEsp <-  cbind(CTP_AG,ProbEsp0
                    ,HL=(CTP0$Hup_RFMP!=0),HU=(CTP0$Hlo_PosEn!=9999))
  
  print(nrow(ProbEsp))
  
  PreFichPE=paste0(substr(obslist[start],1,nchar(obslist[1])-3)
                   ,"_",(end-start+1))
  
  fwrite(ProbEsp,paste0(PreFichPE,"_ProbEsp.csv"))
  
  #for test
  print(list.files(tadir,pattern="_ProbEsp.csv"))
  
}else{
  print("no sound events to classify")
  skip=T
}
