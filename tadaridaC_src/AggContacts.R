library(data.table)
#get arguments from the command line
#args <- commandArgs(trailingOnly = TRUE)

if(length(args)<2)
{
args="ProbEspHF2019-11-28.csv" #probabibility matrix file or directory containing "XXXProbEsp.csv" files
args[6]=F #TC
args[10]="SpeciesList.csv" #species list
load("ClassifEsp_RSDB_HF_tabase3HF_sansfiltre_2019-11-19.learner")
}

Version=5 #allow to track results from different classifier versions


if(exists("r")) #very dirty...
{
  #FichPE=list.files(args[1],pattern="ProbEsp",full.names=T)
  #num=r+ceiling(as.numeric(args[14])/as.numeric(args[7]))*(as.numeric(args[15])-1)
  #PreFichPE=substr(FichPE[[num]],1,nchar(FichPE[[num]])-11)
  #ProbEsp=as.data.frame(fread(FichPE[[num]]))
  ProbEsp=as.data.frame(ProbEsp)
  
  tadir=args[1]
}else{
  
  if((dir.exists(args[1])))
  {
    FichPE=list.files(args[1],pattern="ProbEsp",full.names=T)
    if(length(FichPE)>0)
    {
      my.data <- list()
      # for(f in 1:length(obslist)) {
      for(f in 1:length(FichPE)) {   #0.026 sec/files
        my.data[[f]] <- fread(FichPE[[f]])
      }
      Sys.time()
      
      ProbEsp=as.data.frame(rbindlist(my.data))
    }
    tadir=args[1]
  }else{
    FichPE=args[1]
    ProbEsp=as.data.frame(fread(args[1]))
    if(grepl("/",args[1]))
    {
      tadir=dirname(args[1])
    }else{
      tadir=getwd()
    }
  }
}
#if (length(FichPE)>0)
#{
  
  SpeciesList=fread(args[10])
  
  TestPE_Sp=match(ClassifEspA$classes,colnames(ProbEsp))
    #           ,match(SpeciesList$Esp,colnames(ProbEsp)))
  
#  ColPE_Sp=unique(TestPE_Sp[!is.na(TestPE_Sp)])
 # ColPE_Sp=ColPE_Sp[order(ColPE_Sp)]
  ColPE_Sp=TestPE_Sp
  ColSp1=min(TestPE_Sp)
  
  
  #get the predictions and the main features (noticeably the file name)
  #Loop init
  #this loop intends to detect successively different species within each file if there is sufficient dicrepancy in predicted probabilities
  j=0
  if(exists("IdTot")==T){rm(IdTot)}
  StopLoop=F
  while (nrow(ProbEsp)>0)
  {
    j=j+1
    
    print(paste(j,nrow(ProbEsp),Sys.time()))
    #get the best score per species and file
    ProbEspSp=subset(ProbEsp,select=ColPE_Sp)
    MaxparFich<-aggregate(ProbEspSp,by=list(ProbEsp$Filename),FUN=max)
    
    #find the most probable species in each file
    SpMax<-max.col(MaxparFich[,2:ncol(MaxparFich)],ties.method = "first")
    SpMax2=cbind(Filename=as.character(MaxparFich[,1]),Id=colnames(MaxparFich)[SpMax+1],numsp=SpMax)
    
    #get the probabilities associated to the most probable species in each file
    ProbEsp2=merge(ProbEsp,SpMax2)
    ProbEspDom0=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))
    for (i in 1:(ncol(MaxparFich)))
    {
      subtemp=subset(ProbEsp2,ProbEsp2$numsp==i)
      Probtemp=cbind(subtemp,subtemp[,(i+ColSp1-1)])
      ProbEspDom0=rbind(ProbEspDom0,Probtemp)
    }
    
    #PED = probability of the "most probable species"
    colnames(ProbEspDom0)[ncol(ProbEspDom0)]="PED"
    
    #this probability is converted into a "secondary species score, adding penalties for harmonics and long duration DSEs
    ScoreSec=(-1.5+32.7*ProbEspDom0$PED
              +0.696*ProbEspDom0$HU+0.459*ProbEspDom0$HL)
    
    
    #here sound events are separated in two groups:
    #those whose "most probable species" score is under 2%, and thus are considered to be from other species to be identified in next rounds of the loop (hence go in "ProbEsp")
    #the others are considered to be from the same source and thus are used to compute the probability distribution among species (MaxParFichN1) and ancillary data (median frequency, time of start an time of end during the file)
    
    ProbEspN1=subset(ProbEspDom0,(ScoreSec>0))
    if(nrow(ProbEspN1)==0) #to treat rare cases of low probabilities for all "species"
    {
      ProbEspN1=ProbEspDom0 
      StopLoop=T #to stop the loop (because probabilities went too low in that case)
      
    }
    
    ProbEspSpN1=subset(ProbEspN1,select=ColPE_Sp)
    MaxparFichN1<-aggregate(ProbEspSpN1,by=list(ProbEspN1$Filename),FUN=max)
    
    FreqMed1=aggregate((ProbEspN1$Fmin+ProbEspN1$BW/2),by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5))) #median frequency of sound events
    
    TDeb1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) floor(min(x/100))/10)  #time of the first sound event
    
    TFin1=aggregate((ProbEspN1$StTime+ProbEspN1$Dur),by=list(ProbEspN1$Filename),function(x) ceiling(max(x/100))/10) #time of the last sound event
    
    if("FreqMP" %in% colnames(ProbEspN1))
    {
      FreqPic1=aggregate(ProbEspN1$FreqMP,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5),digits=1))$x #median of peak frequency
    }else{
      FreqPic1=rep(999,nrow(FreqMed1))
    }
    
    
    if("CM_FIF" %in% colnames(ProbEspN1))
    {
      FreqC1=aggregate(ProbEspN1$CM_FIF,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5),digits=1))$x #median of peak frequency
    }else{
      FreqC1=rep(999,nrow(FreqMed1))
    }
    
    SR=aggregate(ProbEspN1$SampleRate,by=list(ProbEspN1$Filename),function(x) x[1])$x
    
    #Calculating number of calls (trying to suppress echoes on the basis of small IPI)
    
    NbCris1=aggregate(ProbEspN1$PrevSt,by=list(ProbEspN1$Filename)
                      ,FUN=function(x) length(subset(x,x>(quantile(x,0.5)/2))))
    
    
    Dur50=aggregate(ProbEspN1$Dur,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5))) #median of sound event duration
    
    Dur90=aggregate(ProbEspN1$Dur,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.9))) #90 percentile of sound event duration
    
    
    if("Amp1" %in% colnames(ProbEspN1))
    {
      
      Ampmax=pmax(ProbEspN1$Amp1,ProbEspN1$Amp2,ProbEspN1$Amp3,ProbEspN1$Amp4) #max amplitude among 4 quarters (in time)
      
      Ampm50=aggregate(Ampmax,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5),digits=1)) #median of Ampmax (see above)
      
      Ampm90=aggregate(Ampmax,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.9),digits=1)) #90 percentile of Ampmax (see above)
      
      AmpSD=apply(cbind(ProbEspN1$Amp1,ProbEspN1$Amp2,ProbEspN1$Amp3,ProbEspN1$Amp4)
                  ,MARGIN=1,FUN=sd)#sd amplitude among 4 quarters (in time)
      
      AmpSMd=aggregate(AmpSD,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5))) #median of AmpSD (see above)
      
      AmpE50=aggregate(ProbEspN1$Amp4,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5),digits=1)) #median amplitude of the last quarter
      
      DiffME=Ampm50$x-AmpE50$x
    }else{
      DiffME=rep(999,nrow(FreqMed1))
      AmpSMd=data.frame(x=rep(999,nrow(FreqMed1)))
      Ampm50=data.frame(x=rep(999,nrow(FreqMed1)))
      Ampm90=data.frame(x=rep(999,nrow(FreqMed1)))
      
    }
    
    
    #storing results
    IdTemp=cbind(MaxparFichN1,FreqM=FreqMed1$x,FreqP=FreqPic1,FreqC=FreqC1
                 ,Tstart=TDeb1$x,Tend=TFin1$x,NbCris=NbCris1$x
                 ,DurMed=Dur50$x,Dur90=Dur90$x,Ampm50=Ampm50$x
                 ,Ampm90=Ampm90$x,AmpSMd=AmpSMd$x,DiffME=DiffME,SR=SR
                 ,Order=paste0("N",j))
    if(exists("IdTot")==T){IdTot=rbind(IdTot,IdTemp)}else{IdTot=IdTemp}
    
    #sound events kept for the next round
    #if "ScoreSec" is negative   
    ProbEsp=subset(ProbEspDom0[,1:(ncol(ProbEspDom0)-3)],ScoreSec<0)
    
    if(StopLoop)
    {ProbEsp=ProbEsp[0,]}  
  }
  
  
  #TestIT_Sp=c(match(SpeciesList$Nesp,colnames(IdTot))
   #           ,match(SpeciesList$Esp,colnames(IdTot)))
  
  #ColIT_Sp=unique(TestIT_Sp[!is.na(TestIT_Sp)])
   #ColIT_Sp=ColIT_Sp[order(ColIT_Sp)]
  ITSp=subset(IdTot,select=ClassifEspA$classes)
  
  
  #computing the species
  SpMaxF<-max.col(ITSp,ties.method = "first")
  SpMaxF2=colnames(ITSp)[SpMaxF]
  IdTot$Ind=apply(ITSp,MARGIN=1,FUN=max)
  IdTot$SpMaxF2=SpMaxF2
  IdTot$Version=Version
  
  if(exists("r"))
  {
    fwrite(IdTot,paste0(PreFichPE,"IdTot.csv"))
    fwrite(cbind(Filename=IdTot[,1],IdTot[,((ncol(IdTot)-18):ncol(IdTot))]),paste0(PreFichPE,"Idshort.csv"))
  }else{
    fwrite(IdTot,paste0(tadir,"/IdTot.csv"))
    fwrite(cbind(Filename=IdTot[,1],IdTot[,((ncol(IdTot)-18):ncol(IdTot))]),paste0(tadir,"/Idshort.csv"))
  }
  
  #rm(list=setdiff(ls(), list("ClassifEspA")))
  
#}else{
#  print("no sound events to aggregate")
#}

  