##BEWARE experimental version, implementing new confidence index and new "secondary species" algorithm. Still being tested...

options(error = function() traceback(2))

#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
#uncomment the following line if you prefer to do not use R in command line
#args=choose.dir()
args="E:\\PI_NotreDameDeLondresJardin180212x1\\txt"

if(length(args)==0){
  print("usage: Rscript TadaridaC.r <directory>")
  q()
}

tadir=args[1]
library(randomForest) #to use the classifier
library(data.table) #to handle large numbers of .ta files

Version=3 #allow to track results from different classifier versions

#get the .ta files list
#obslist=list.files(tadir,pattern=".ta$",full.names=T,recursive=F)
obslist=list.files(tadir,pattern=".ta$",full.names=T,recursive=T)
if (length(obslist) == 0) {
  print("no .ta files to process")
  q()
}

Sys.time()

block=2000

l<-length(obslist)
nruns<-ceiling(l/block)


for (r in 1:nruns){
  print(paste(r,"/",nruns,Sys.time()))
  fstart<-(r*block)-block+1
  fend<-(r*block)
  fend<-ifelse(fend>l,l,fend)
  my.data <- list()
  for(f in fstart:fend) {
    my.data[[f]] <- read.csv(obslist[[f]],sep="\t")
  }
  





# load the classifier
if (exists("ClassifEspA")==FALSE) load("ClassifEsp_LF_180209.learner")

#concatenate all the features table
CTP=as.data.frame(rbindlist(my.data))
summary(CTP$FreqMP)
CTPb=subset(CTP,CTP$FreqMP<=0.8)
#CTP=subset(CTP,CTP$FreqMP>0.8)


#get the predictions and the main features (noticeably the file name)
ProbEsp0 <- predict(ClassifEspA, CTP,type="prob",norm.votes=TRUE)



#Loop init
ProbEsp <-  cbind(CTP[,1:12],ProbEsp0
                  ,HL=(CTP$Hup_RFMP!=0),HU=(CTP$Hlo_PosEn!=9999))
#this loop intends to detect successively different species within each file if there is sufficient dicrepancy in predicted probabilities
j=0
while (nrow(ProbEsp)>0)
 {
  j=j+1
  
  #get the best score per species and file
  MaxparFich<-aggregate(ProbEsp[,13:((ncol(ProbEsp)-2))],by=list(ProbEsp$Filename),FUN=max)
  
  #find the most probable species in each file
  SpMax<-max.col(MaxparFich[,2:ncol(MaxparFich)],ties.method = "first")
  SpMax2=cbind(Filename=as.character(MaxparFich[,1]),Id=colnames(MaxparFich)[SpMax+1],numsp=SpMax)
  
  #get the probabilities associated to the most probable species in each file
  ProbEsp2=merge(ProbEsp,SpMax2)
  ProbEspDom0=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))
  for (i in 1:(ncol(MaxparFich)))
  {
    subtemp=subset(ProbEsp2,ProbEsp2$numsp==i)
    Probtemp=cbind(subtemp,subtemp[,(i+12)])
    ProbEspDom0=rbind(ProbEspDom0,Probtemp)
  }
  
  #PED = probability of the "most probable species"
  colnames(ProbEspDom0)[ncol(ProbEspDom0)]="PED"
  
  #this probability is converted into a "secondary species score, adding penalties for harmonics and long duration DSEs
  ScoreSec=(-1.3+32.7*ProbEspDom0$PED
            +0.696*ProbEspDom0$HU+0.459*ProbEspDom0$HL
            -2.98*ProbEspDom0$PED*log(ProbEspDom0$Dur+1))
  
  #computing minimum, second and fifth score among DSEs
  ScoreM=aggregate(ScoreSec,by=list(ProbEspDom0$Filename)
                   ,FUN=min)
  
  Score2=aggregate(ScoreSec,by=list(ProbEspDom0$Filename)
                   ,FUN=function(x) 
                     if(length(x)>1){quantile(x,2/length(x))}else{max(x)})
  
  Score5=aggregate(ScoreSec,by=list(ProbEspDom0$Filename)
                   ,FUN=function(x) 
                     if(length(x)>4){quantile(x,5/length(x))}else{max(x)})
  
  ScoreNClust=aggregate(ScoreSec,by=list(ProbEspDom0$Filename)
                   ,FUN=function(x) 
                     length(subset(x,x<0))-length(subset(x,((x>0)&(x<1)))))
  
  #allowing to compute a "secondary species index"
  
  SecInd=1.17+1.6*ScoreM$x+0.0574*Score2$x+0.0312*Score5$x
  
  #here sound events are separated in two groups:
  #those whose "most probable species" score is under 2%, and thus are considered to be from other species to be identified in next rounds of the loop (hence go in "ProbEsp")
  #the others are considered to be from the same source and thus are used to compute the probability distribution among species (MaxParFichN1) and ancillary data (median frequency, time of start an time of end during the file)
  
  ProbEspN1=subset(ProbEspDom0,(ScoreSec>0))
  if(nrow(ProbEspN1)==0) #to treat rare cases of low probabilities for all "species"
  {
    ProbEspN1=ProbEspDom0 
    SecInd=1 #to stop the loop (because probabilities went too low in that case)
    }
  MaxparFichN1<-aggregate(ProbEspN1[,13:(ncol(ProbEspN1)-5)],by=list(ProbEspN1$Filename),FUN=max) #matrix of maximum probability per species
  
  FreqMed1=aggregate((ProbEspN1$Fmin+ProbEspN1$BW/2),by=list(ProbEspN1$Filename),function(x) floor(quantile(x,0.5))) #median frequency of sound events
  
  TDeb1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) floor(min(x/100))/10)  #time of the first sound event
  
  TFin1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) ceiling(max(x/100))/10) #time of the last sound event
  
  #storing results
  IdTemp=cbind(MaxparFichN1,FreqM=FreqMed1$x,Tstart=TDeb1$x,Tend=TFin1$x,Order=paste("N",j,sep=""))
  if(exists("IdTot")==T){IdTot=rbind(IdTot,IdTemp)}else{IdTot=IdTemp}
  
  #sound events kept for the next round
  #3 conditions: 
  #condition 1: they belong to a file whose "SecInd" is negative   
  ProbEsp=subset(ProbEspDom0[,1:(ncol(ProbEspDom0)-3)],ScoreSec<0)
  ProbEsp=merge(ProbEsp,subset(ScoreM,SecInd<0),by.x="Filename",by.y="Group.1")
  ProbEsp=ProbEsp[,1:(ncol(ProbEsp)-1)]
  #condition2: SecInd should be negative (see above)
  if(min(SecInd)>0)
  {ProbEsp=ProbEsp[0,]}else{
    ProbEsp=merge(ProbEsp,subset(ScoreM,SecInd<0),by.x="Filename",by.y="Group.1")
    ProbEsp=ProbEsp[,1:(ncol(ProbEsp)-1)]
    #condition3: lowest values of ScoreSec form a distinctive cluster
    ProbEsp=merge(ProbEsp,subset(ScoreNClust,ScoreNClust$x>0),by.x="Filename",by.y="Group.1")
    ProbEsp=ProbEsp[,1:(ncol(ProbEsp)-1)]
  }
  print(j)
    }


#computing the species
SpMaxF<-max.col(IdTot[,2:(ncol(IdTot)-4)],ties.method = "first")
SpMaxF2=colnames(MaxparFich)[SpMaxF+1]

#Calculating the confidence index (linear combination of the four highest scores)
n1=apply(IdTot[,2:(ncol(IdTot)-4)],MARGIN=1,FUN=max)

n2=apply(IdTot[,2:(ncol(IdTot)-4)],MARGIN=1
         ,FUN=function(x) quantile(x,((ncol(IdTot)-7))/(ncol(IdTot)-5),type=1))
n3=apply(IdTot[,2:(ncol(IdTot)-4)],MARGIN=1
         ,FUN=function(x) quantile(x,((ncol(IdTot)-8))/(ncol(IdTot)-5),type=1))
n4=apply(IdTot[,2:(ncol(IdTot)-4)],MARGIN=1
         ,FUN=function(x) quantile(x,((ncol(IdTot)-9))/(ncol(IdTot)-5),type=1))
NIC=exp(-2.5+8*n1-2.5*(n2+n3+n4))/(1+exp(-2.5+8*n1-2.5*(n2+n3+n4)))


IdDetail=cbind(IdTot,SpMaxF2,NIC,n1,n2,n3,n4)

IdDetail=IdDetail[order(IdDetail$Group.1),]

Detail=subset(IdDetail,select=c(Group.1,FreqM:n4))

fichierid=paste(args,r,"IdDetailNEW.csv",sep="")
write.table(IdDetail,fichierid,row.names=FALSE,sep=";")
barplot(table(IdDetail$SpMaxF2),las=2,ylim=c(0,500))
f2p <- function(x)
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 22, nchar(x)-8), ".", substr(x, nchar(x) - 6, nchar(x)-4), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS")
}
Date=f2p(as.character(IdDetail$Group.1))
Time=(Date$hour*3600+Date$min*60+Date$sec)/3600+24*(1-floor(Date$hour/12))
stripchart(as.numeric(as.character(IdDetail$NIC))~IdDetail$SpMaxF2,las=2,cex.axis=0.65,cex=0.65)
axis(4,at=c(1:length(levels(IdDetail$SpMaxF2))),labels=levels(IdDetail$SpMaxF2),las=2,cex.axis=0.65,cex=0.65)
stripchart(Time~IdDetail$SpMaxF2,las=2,cex.axis=0.65,cex=0.65)

axis(4,at=c(1:length(levels(IdDetail$SpMaxF2))),labels=levels(IdDetail$SpMaxF2),las=2,cex.axis=0.65,cex=0.65)
gc()

AgB1=aggregate(CTPb$FreqMP,by=list(CTPb$Filename),FUN=mean)
AgB2=aggregate(CTPb$FreqMP,by=list(CTPb$Filename),FUN=length)
AgB3=aggregate(CTPb$Dur,by=list(CTPb$Filename),FUN=max)
AgB=cbind(AgB1,AgB2$x,AgB3$x)
write.csv(AgB,paste(args,r,"AgB.csv",sep=""),row.names=F)


rm(my.data)
}

rm(IdDetail)

#IdMax=aggregate(subset(IdTot,select=Acrsch:wings),by=list(IdTot$Group.1),FUN=max)

#IdTot3=merge(IdTot,IdMax,by="Group.1")

#write.csv(IdTot3,paste("IdTot3",r,".csv"),row.names=F)

