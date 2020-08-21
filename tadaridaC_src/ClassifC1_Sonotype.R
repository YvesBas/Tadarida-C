options(error = function() traceback(2))

Var_AGarder=c("Filename","CallNum","Version","FileDur","SampleRate"
              ,"StTime","Dur","PrevSt","Fmax","Fmin","BW"
              ,"FreqMP","CM_FIF","Amp1","Amp2","Amp3","Amp4", 
              "Hup_AmpDif", "Hlo_AmpDif", "CO2_FIF")

#get arguments from the command line
#args <- commandArgs(trailingOnly = TRUE)
#uncomment the following line if you prefer to do not use R in command line
#args="E:/waves/wav180225/txt"
#args[2]="ClassifEsp_LF_180320.learner"
#args[2]="ClassifEspFrance180303.learner"
#args[3]="tabase3_LFXC"
#args[3]="NA"

#options (HPF = filtre passe-haut / Reduc = réduction des features par DFA)
#args[4]=8 #HPF
#args[8]=1 #start number
#args[9]=100 #end number number
obslist=talistot # Lists of .ta files to classify
FIO=FITA # file.info(talistot)
#obslist=list.files(args[1],pattern=".ta$",full.names=T,recursive=T) # 1e-4 sec/files
#args[15]="ClassifEsp_tabase3HF_France_Cir_2019-11-26_wiSR.learner"


skip=F

if(length(args)==0){
  print("usage: Rscript TadaridaC.r <directory>")
  q()
}

library(randomForest) #to use the classifier
library(data.table) #to handle large numbers of .ta files
library(MASS) # to handle reduction (lda predict)

#get the .ta files list
if (length(obslist) == 0) {
  print("no .ta files to process")
  q()
}
#FIO=file.info(obslist)
#obslist=subset(obslist,FIO$size>1000)


#fwrite(as.list(obslist),"obslist.csv")

start=max(1,as.numeric(args[8])) # Block start number
end=min(length(obslist),as.numeric(args[9])) # Block end number

# Read sound features (.ta files)
Sys.time()
my.data <- list()
# for(f in 1:length(obslist)) {
for(f in start:end) {   #0.026 sec/files
  #print(obslist[[f]])    
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

#print(ls())

#concatenate all the features table
CTP=as.data.frame(rbindlist(my.data,use.names=T,fill=T))

#fwrite(CTP,"CTP.csv")
#test=subset(CTP,CTP$Filename=="Car791428-2016-Pass1-Z4-MU101529-SM303373_0_20160622_002538_000.wav")

#discard DSE below 8 kHz or 0.8 kHz
if(is.na(match("FreqMP",names(CTP)))==F)
{
  CTP=subset(CTP,CTP$FreqMP>as.numeric(args[4]))
}

#discard DSE suspected to be a harmonic 
# For lower harmonics
if(is.na(match("Hlo_AmpDif",names(CTP)))==F)
{
  CTP=subset(CTP,CTP$Hlo_AmpDif ==0)
}
# For upper harmonics
if(is.na(match("Hup_AmpDif",names(CTP)))==F)
{
  CTP=subset(CTP,CTP$Hup_AmpDif ==0)
}

#discard DSE suspected to be from the same call as next DSE
#but separated due to low quality
CTP$PrevStMinDur=999
for (i in 2:nrow(CTP)){
  {
    CTP$PrevStMinDur[i-1]=CTP$PrevSt[i]-CTP$Dur[i-1]
  }
}

CTP=subset(CTP, CTP$PrevStMinDur>5)

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
    #plot(test2,tabaseR[,1])
    CTP[,i]=test2
    #fwrite(list(i),"i.csv")
    #print(i)
  }
  
  
  Reduc2=predict(Scaling,CTP[,4:274])
  
  CTP0=cbind(CTP0,Reduc2$x)
}


#fwrite(CTP0,"CTP0.csv")

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
  
  #fwrite(as.data.frame(ProbEsp0),"ProbEsp0.csv")
  
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
