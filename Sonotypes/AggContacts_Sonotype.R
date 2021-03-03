library(data.table)
library(dplyr)
library(Hmisc)

Version=args[22] #allow to track results from different classifier versions

# Function find_modes
find_modes<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] >= x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}

if(exists("r")) #very dirty...
{
  ProbEsp=as.data.frame(ProbEsp)
  tadir=args[1]
}else{
  
  if((dir.exists(args[1])))
  {
    FichPE=list.files(args[1],pattern="ProbEsp",full.names=T) # Lists prediction files
    
    # Read ProbEsp file
    if(length(FichPE)>0)
    {
      my.data <- list()
      for(f in 1:length(FichPE)) {   #0.026 sec/files
        my.data[[f]] <- fread(FichPE[[f]])
      }
      Sys.time()
      
      # Rbind ProbEsp files if there are several
      ProbEsp=as.data.frame(rbindlist(my.data))
    }
    tadir=args[1]
    
    # If arg[1] is not a directory but a file, loads the file directly
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

#### Reads species list file ####
SpeciesList=fread(args[10], header=T)

TestPE_Sp=c(match(SpeciesList$Nesp,colnames(ProbEsp))
            ,match(SpeciesList$Esp,colnames(ProbEsp)))

# ColPE_Sp refers to column numbers of each Sonotype
ColPE_Sp=unique(TestPE_Sp[!is.na(TestPE_Sp)]) 
ColPE_Sp=ColPE_Sp[order(ColPE_Sp)]
ColSp1=min(ColPE_Sp)

# Test of conformity
TestConform=match(colnames(ProbEsp)[ColSp1:length(ProbEsp)],SpeciesList$Esp)
if(sum(is.na(TestConform[1:(length(TestConform)-2)]))>0)
{
  print(subset(colnames(ProbEsp)[ColSp1:length(ProbEsp)],is.na(TestConform)))
  stop("conformity problem between classifier and species list (missing species)")
}

#get the predictions and the main features (noticeably the file name)
#### Loop init ####
#this loop intends to detect successively different species within each file if 
#there is sufficient discrepancy in predicted probabilities
j=0
if(exists("IdTot")==T){rm(IdTot)}
StopLoop=F
while (nrow(ProbEsp)>0)
{
  j=j+1
  
  print(paste(j,nrow(ProbEsp),Sys.time()))
  
  # Identifies each call with the best sonotype probability
  
  #### get the best score per species and file ####
  ProbEspSp=subset(ProbEsp,select=ColPE_Sp)
  MaxparFich<-aggregate(ProbEspSp,by=list(ProbEsp$Filename),FUN=max)
  MaxparFich$ScoreMax=apply(MaxparFich[,2:ncol(MaxparFich)],MARGIN=1,max)
  
  #find the most probable species in each file
  SpMax<-max.col(MaxparFich[,2:ncol(MaxparFich)],ties.method = "first")
  SpMax2=cbind(Filename=as.character(MaxparFich[,1]),
               Id=colnames(MaxparFich)[SpMax+1],
               numsp=SpMax)
  
  #get the probabilities associated to the most probable species in each file
  ####PED = probability of the "most probable species"####
  ProbEsp2=merge(ProbEsp,SpMax2)
  ProbEspDom0=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))
  for (i in 1:(ncol(MaxparFich)))
  {
    subtemp=subset(ProbEsp2,ProbEsp2$numsp==i)
    Probtemp=cbind(subtemp,subtemp[,(i+ColSp1-1)])
    ProbEspDom0=rbind(ProbEspDom0,Probtemp)
  }
  colnames(ProbEspDom0)[ncol(ProbEspDom0)]="PED"
  
  #get the call with maximum probability and keep the parameter for best mode
  ProbM=left_join(ProbEspDom0,MaxparFich,by=c("Filename"="Group.1"))
  ProbCallMax=subset(ProbM,ProbM$PED==ProbM$ScoreMax)
  ProbCallMax=ProbCallMax[!duplicated(ProbCallMax$Filename),]
  
  #If there is more than 1 call per file, proceed with sorting, else go directly at storing
  #If more than one call per file
  ProbEspDom01=ProbEspDom0[(duplicated(ProbEspDom0$Filename) | 
                              duplicated(ProbEspDom0$Filename, fromLast = TRUE)), ]
  #If only one call per file
  ProbEspDom_1call=ProbEspDom0[!(duplicated(ProbEspDom0$Filename) | 
                                   duplicated(ProbEspDom0$Filename, fromLast = TRUE)), ]
  
  #Identify each call belonging to the dominant mode within the dominant sonotype
  #Subset rows belonging to the dominant sonotype
  PourModeTemp=subset(ProbEspDom01, ProbEspDom01$PED>as.numeric(args[20]))
  ListFilenames=names(table(PourModeTemp$Filename))
  
  if(!is.null(ListFilenames)){
    
  #### Find the dominant mode of the peak frequency ####
  if(exists("Modes")==T){rm(Modes)}
  if(exists("ModeInf")==T){rm(ModeInf)}
  if(exists("ModeSup")==T){rm(ModeSup)}
  for (k in 1:length(ListFilenames))
  {

    PourModeTemp2sub=subset(PourModeTemp, PourModeTemp$Filename==ListFilenames[k])
    if(nrow(PourModeTemp2sub)>1){
      Density_FreqMP=density(PourModeTemp2sub[,args[19]], adjust=0.3)
      if(is.numeric(find_modes(Density_FreqMP$y))){
        modesFC=Density_FreqMP$x[find_modes(Density_FreqMP$y)]
        
        # Find mode closest to the parameter of the call with highest PED
        findmode=find.matches(ProbCallMax[,args[19]][which(ProbCallMax$Filename==ListFilenames[k])],
                              modesFC, 
                              tol=200,
                              maxmatch=1)
        mainmode=modesFC[findmode$matches]
        
      }else{ # if returns 'This is a monotonic distribution'
        modesFC=mean(PourModeTemp2sub[,args[19]])
        mainmode=modesFC
      }}else{
        modesFC=mean(PourModeTemp2sub[,args[19]])
        mainmode=modesFC
      } 
    
    mainmode2=which(modesFC==mainmode)
    
    #### PLOT DEBUG will run for the file provided in arg[21] ####
    if(!is.na(args[21]==1)){
    if(ListFilenames[k]==gsub("ta", "wav", args[21]))
    {
      print(paste("j=", j, "k=", k, "mainmode=", mainmode, "modesFC=",  
                  unique(PourModeTemp2sub$Id), sep=" "))
      print(modesFC)
      print(plot(Density_FreqMP, 
                 main = paste (j, k, unique(PourModeTemp2sub$Id))),
            axis(1, seq(0,200,10)))
      print(cbind(PourModeTemp2sub[,args[19]], PourModeTemp2sub$Id))
    }
    }
    
    #Defines mode inferior threshold with a tolerance of 5 kHz from main mode
    ModeInfTemp = modesFC[mainmode2]-5
    
    #Defines mode superior threshold with a tolerance of 5 kHz from main mode
    ModeSupTemp = modesFC[mainmode2]+ 5
    
    if(exists("Modes")==T){Modes=c(Modes, mainmode)}else{Modes=mainmode}
    if(exists("ModeInf")==T){ModeInf=c(ModeInf, ModeInfTemp)}else{ModeInf=ModeInfTemp}
    if(exists("ModeSup")==T){ModeSup=c(ModeSup, ModeSupTemp)}else{ModeSup=ModeSupTemp}
  }

  
  # Store the dominant mode of the peak frequency for the dominant sonotype
  FileMode=as.data.frame(cbind(ListFilenames, Modes, ModeInf, ModeSup))
  colnames(FileMode)=c("Filename", "MainMode", "ModeInf", "ModeSup")
  FileMode$MainMode=as.numeric(as.character(FileMode$MainMode))
  FileMode$ModeInf=as.numeric(as.character(FileMode$ModeInf))
  FileMode$ModeSup=as.numeric(as.character(FileMode$ModeSup))
  ProbEspDom1=merge(ProbEspDom01,FileMode, all.x = T)
  ProbEspDom1$MainMode[which(is.na(ProbEspDom1$MainMode))]=ProbEspDom1[,args[19]][which(is.na(ProbEspDom1$MainMode))]
  ProbEspDom1$ModeInf[which(is.na(ProbEspDom1$ModeInf))]=-999
  ProbEspDom1$ModeSup[which(is.na(ProbEspDom1$ModeSup))]=999
  
  #Assess whether each call belongs to the dominant frequency mode of the dominant sonotype
  ProbEspDom1$IsDominant= ifelse(ProbEspDom1[,args[19]]< ProbEspDom1$ModeSup &
                                   ProbEspDom1[,args[19]]> ProbEspDom1$ModeInf, T, F)
  
  
  ####sound events are separated in two groups: ################################
  
  #(1) those whose "most probable species" score is > args[20]
  #AND belong to the dominant frequency mode
  #(hence go in "ProbEspN1")
  #they are considered to be from the same source and thus are used to 
  #compute the probability distribution among species (MaxparFichN1) 
  #and ancillary data (median frequency, time of start and time of end during the file)
  
  #(2) those whose "most probable species" score is < args[20]
  #OR not belonging to the dominant frequency mode
  #(hence go in "ProbEsp")
  #they are considered to be from other species 
  #they are to be identified in next rounds of the loop 
  
  ProbEspN1_0=subset(ProbEspDom1,IsDominant & ProbEspDom1$PED>as.numeric(args[20]))
  
  #Add files that only contained 1 call and take peak frequency as dominant mode
  ProbEspN1 = bind_rows (ProbEspN1_0, ProbEspDom_1call)
  ProbEspN1$MainMode[is.na(ProbEspN1$MainMode)]=ProbEspN1[,args[19]][is.na(ProbEspN1$MainMode)]
  ProbEspN1$IsDominant[is.na(ProbEspN1$IsDominant)]=TRUE
  
  }else{
    ProbEspN1=ProbEspDom_1call
    ProbEspN1$MainMode=ProbEspN1[,args[19]]
    ProbEspN1$IsDominant=TRUE
      }
  
  #to treat rare cases of low probabilities for all "species"
  if(nrow(ProbEspN1)==0) 
  {
    ProbEspN1=ProbEspDom1 
    StopLoop=T #to stop the loop (because probabilities went too low in that case)
    
  }
  
  #sound events kept for the next round ######################################
  #if "PED" is < args[20] OR if not belonging to the dominant frequency mode
  
  ProbEsp=subset(ProbEspDom1[,1:(ncol(ProbEspDom1)-7)],ProbEspDom1$PED<as.numeric(args[20]) | 
                   !ProbEspDom1$IsDominant)
  
  ProbEspSpN1=subset(ProbEspN1,select=ColPE_Sp)
  ProbEspSpN1$MainMode=round(ProbEspN1$MainMode, digits=1)
  MaxparFichN1<-aggregate(ProbEspSpN1,by=list(ProbEspN1$Filename),FUN=max)
  
  #### Compute ancillary data ####
  FreqMed1=aggregate((ProbEspN1$Fmin+ProbEspN1$BW/2),by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5))) #median frequency of sound events
  TDeb1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) floor(min(x/100))/10)  #time of the first sound event
  TFin1=aggregate((ProbEspN1$StTime+ProbEspN1$Dur),by=list(ProbEspN1$Filename),function(x) ceiling(max(x/100))/10) #time of the last sound event
  
  # Peak Frequency
  if("FreqMP" %in% colnames(ProbEspN1))
  {
    FreqPic1=aggregate(ProbEspN1$FreqMP,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5),digits=1))$x #median of peak frequency
  }else{
    FreqPic1=rep(999,nrow(FreqMed1))
  }
  
  #
  if("CM_FIF" %in% colnames(ProbEspN1))
  {
    FreqC1=aggregate(ProbEspN1$CM_FIF,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5),digits=1))$x #median of peak frequency
  }else{
    FreqC1=rep(999,nrow(FreqMed1))
  }
  
  SR=aggregate(ProbEspN1$SampleRate,by=list(ProbEspN1$Filename),function(x) x[1])$x
  
  # Number of calls (trying to suppress echoes on the basis of small IPI)
  NbCris1=aggregate(ProbEspN1$PrevSt,by=list(ProbEspN1$Filename)
                    ,FUN=function(x) length(subset(x,x>(quantile(x,0.5)/2))))
  
  Dur50=aggregate(ProbEspN1$Dur,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.5))) #median of sound event duration
  Dur90=aggregate(ProbEspN1$Dur,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.9))) #90 percentile of sound event duration
  
  # Amplitudes (proxies for SNR)
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
  
  # Harmonics
  # Amplitude ratio between DSE and potential harmonics
  MaxRamp=pmax(ProbEspN1$Ramp_1_2,ProbEspN1$Ramp_2_1,ProbEspN1$Ramp_3_2,
                         ProbEspN1$Ramp_4_3,ProbEspN1$Ramp_2_3)
  MaxRamp2=colnames(ProbEspN1[,c("Ramp_1_2",
                                           "Ramp_2_1","Ramp_3_2",
                                           "Ramp_4_3","Ramp_2_3")])[max.col(ProbEspN1[,c("Ramp_1_2",
                                                                                        "Ramp_2_1",
                                                                                        "Ramp_3_2",
                                                                                        "Ramp_4_3",
                                                                                        "Ramp_2_3")],ties.method = "first")]
  Ramp90=aggregate(MaxRamp,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.9),digits=1)) #90 percentile of MaxRamp
  
  # Energy ratio between potential harmonic and surrounding noise
  # MaxRAN=pmax(ProbEspN1$RAN_1_2,ProbEspN1$RAN_2_1,ProbEspN1$RAN_3_2,
  #              ProbEspN1$RAN_4_3,ProbEspN1$RAN_2_3)
  # MaxRAN2=colnames(ProbEspN1[,c("RAN_1_2",
  #                                "RAN_2_1","RAN_3_2",
  #                                "RAN_4_3","RAN_2_3")])[max.col(ProbEspN1[,c("RAN_1_2","RAN_2_1",
  #                                                                              "RAN_3_2","RAN_4_3",
  #                                                                              "RAN_2_3")],ties.method = "first")]
  # RAN90=aggregate(MaxRAN,by=list(ProbEspN1$Filename),function(x) round(quantile(x,0.9),digits=1)) #90 percentile of MaxRamp
  # 

  #storing results
  IdTemp=cbind(MaxparFichN1,FreqM=FreqMed1$x,FreqP=FreqPic1,FreqC=FreqC1
               ,Tstart=TDeb1$x,Tend=TFin1$x,NbCris=NbCris1$x
               ,DurMed=Dur50$x,Dur90=Dur90$x,Ampm50=Ampm50$x
               ,Ampm90=Ampm90$x,AmpSMd=AmpSMd$x,DiffME=DiffME,SR=SR
               ,Ramp90=Ramp90$x
               ,Order=paste0("N",j))
  
  if(exists("IdTot")==T){IdTot=rbind(IdTot,IdTemp)}else{IdTot=IdTemp}
  
  
 if(!T %in% duplicated(ProbEspN1$Filename))
 {
   ProbEsp=ProbEsp[0,]
 }

  if(StopLoop)
  {ProbEsp=ProbEsp[0,]}  
}


TestIT_Sp=c(match(SpeciesList$Nesp,colnames(IdTot))
            ,match(SpeciesList$Esp,colnames(IdTot)))

ColIT_Sp=unique(TestIT_Sp[!is.na(TestIT_Sp)])
ColIT_Sp=ColIT_Sp[order(ColIT_Sp)]
ITSp=subset(IdTot,select=ColIT_Sp)


#computing the species
SpMaxF<-max.col(ITSp,ties.method = "first")
SpMaxF2=colnames(ITSp)[SpMaxF]
IdTot$Ind=apply(ITSp,MARGIN=1,FUN=max)
IdTot$SpMaxF2=SpMaxF2
IdTot$Version=Version

IdTot$Filename=IdTot$Group.1
IdTot= IdTot[order(IdTot$Filename, IdTot$SpMaxF2), ]


if(exists("r"))
{
  fwrite(IdTot,paste0(PreFichPE,"IdTot.csv"))
  fwrite(cbind(Filename=IdTot[,1],IdTot[,((ncol(IdTot)-19):ncol(IdTot))]),paste0(PreFichPE,"Idshort.csv"))
}else{
  fwrite(IdTot,paste0(tadir,"/IdTot.csv"))
  fwrite(cbind(Filename=IdTot[,1],IdTot[,((ncol(IdTot)-19):ncol(IdTot))]),paste0(tadir,"/Idshort.csv"))
}

