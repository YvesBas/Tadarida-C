options(error = function() traceback(2))

#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
#uncomment the following line if you prefer to do not use R in command line
args="C:/Users/yves/Documents/Tadarida/Tadarida-C/tests/txt"
if(length(args)==0){
  print("usage: Rscript TadaridaC.r <directory>")
  q()
}
tadir=args[1]

#uncomment the following line if you do not store the classifier file in the working directory of R
#and in that case, indicate the folder path where the classifier file is stored
#setwd("C:/Users/yves/Documents/Tadarida/Tadarida-C/tests")

library(randomForest) #to use the classifier
library(data.table) #to handle large numbers of .ta files

Version=3 #allow to track results from different classifier versions

#get the .ta files list
obslist=list.files(tadir,pattern=".ta$",full.names=T,recursive=F)

if (length(obslist) == 0) {
  print("no .ta files to process")
  q()
}

# load the classifier
if (exists("ClassifEspA")==FALSE) load("ClassifEspHF3.learner")

#concatenate all the features table
my.data <- list()
for (i in 1:length(obslist)){
  my.data[[i]] <- read.csv(obslist[[i]],sep="\t")
}
CTP=as.data.frame(rbindlist(my.data))

#get the predictions and the main features (noticeably the file name)
ProbEsp0 <- predict(ClassifEspA, CTP,type="prob",norm.votes=TRUE)
ProbEsp <-  cbind(CTP[,1:12],ProbEsp0)

#this loop intends to detect successively different species within each file if there is sufficient dicrepancy in predicted probabilities
j=0
while (nrow(ProbEsp)>0)
 {
  j=j+1
  
  #get the best score per species and file
  MaxparFich<-aggregate(ProbEsp[,13:(ncol(ProbEsp))],by=list(ProbEsp$Filename),FUN=max)
  
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
  
  #here sound events are separated in two groups:
  #those whose "most probable species" score is under 2%, and thus are considered to be from other species to be identified in next rounds of the loop (hence go in "ProbEsp")
  #the others are considered to be from the same source and thus are used to compute the probability distribution among species (MaxParFichN1) and ancillary data (median frequency, time of start an time of end during the file)
  
  ProbEspN1=subset(ProbEspDom0,ProbEspDom0$PED>=0.02)
  
  MaxparFichN1<-aggregate(ProbEspN1[,13:(ncol(ProbEspN1)-3)],by=list(ProbEspN1$Filename),FUN=max)
  
  FreqMed1=aggregate((ProbEspN1$Fmin+ProbEspN1$BW/2),by=list(ProbEspN1$Filename),function(x) floor(quantile(x,0.5)))
  
  TDeb1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) floor(min(x/100))/10)
  
  TFin1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) ceiling(max(x/100))/10)
  
  #storing results
  IdTemp=cbind(MaxparFichN1,FreqM=FreqMed1$x,Tstart=TDeb1$x,Tend=TFin1$x,Order=paste("N",j,sep=""))
  if(exists("IdTot")==T){IdTot=rbind(IdTot,IdTemp)}else{IdTot=IdTemp}
  
  #sound events kept for the next round (see above)
  ProbEsp=subset(ProbEspDom0[,1:(ncol(ProbEspDom0)-3)],ProbEspDom0$PED<0.02)
  
}

#adding version number from both Tadarida-D and Tadarida-C
IdTot2=cbind(IdTot,VersionD=CTP$Version[1],VersionC=Version)

#writing .tc files
for (i in 1:nlevels(IdTot2$Group.1))
{
  fichierid=paste(tadir,'/',substr(levels(IdTot2$Group.1)[i],1,(nchar(levels(IdTot2$Group.1)[i])-4)),".tc", sep="")
  write.csv(subset(IdTot2,IdTot2$Group.1==levels(IdTot2$Group.1)[i]),fichierid,row.names=FALSE)  
}


#suppressing every objects except the classifier (which is time-consuming to load)
rm(list=setdiff(ls(), "ClassifEspA"))
