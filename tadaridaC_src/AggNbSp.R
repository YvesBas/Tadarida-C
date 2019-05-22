library(data.table)
library(randomForest)
#library(proxy)
#args="" #init
#args[6]=T #TC files
#args[10]="SpeciesList.csv" #species list
#args[11]="CNS_tabase3HF_France_IdConc.learner" #name of the species number" classifier
#args[12]=T #if species number should be filtered or not


if(dir.exists(args[1]))
{
  #FichIT=list.files(args[1],pattern="IdTot",full.names=T)
  #if (length(FichIT)>0)
  #{
  #  my.data <- list()
  # for(f in 1:length(obslist)) {
  #  for(f in 1:length(FichIT)) {   #0.026 sec/files
  #   my.data[[f]] <- fread(FichIT[[f]])
  #  }
  # Sys.time()
  
  #IdTot=rbindlist(my.data)
  IdTot=as.data.table(IdTot)
  tadir=args[1]
}else{
  IdTot=as.data.frame(fread(paste0(dirname(args[1]),"/IdTot.csv")))
  if(grepl("/",args[1]))
  {
    tadir=dirname(args[1])
  }else{
    tadir=getwd()
  }
}

#if(length(FichIT)>0)
#{
  #get the variables necessary to predict "NbSp"
  IdTot=IdTot[order(IdTot$Group.1,IdTot$Order),]
  
  IdTot$OrderNum=as.numeric(gsub("N","",IdTot$Order))
  IdTot$Duree=IdTot$Tend-IdTot$Tstart
  
  
  if(args[12])
  {
    
    SpeciesList=fread(args[10])
    if(!exists("ClassifNbSp")){load(args[11])}
    
    
    test=subset(IdTot,IdTot$Duree>5)
    
    #test=match("003760_0_20160907_033641_676.wav",levels(as.factor(IdTot$Group.1)))
    
    
    IdTot$Group.1=factor(IdTot$Group.1,exclude=NULL)
    
    Overlap=vector()
    FreqDiff=vector()
    FreqRatio=vector()
    IdTot2=IdTot[0,]
    for (i in 1:nlevels(as.factor(IdTot$Group.1)))
    {
      IdSub=subset(IdTot,IdTot$Group.1==levels(as.factor(IdTot$Group.1))[i])
      TS=5
      TE=0
      FreqDom=999
      for (j in 1:nrow(IdSub))
      {
        TSO=max(TS,IdSub$Tstart[j])
        TEO=min(TE,IdSub$Tend[j])
        OverlapTemp=max(0,TEO-TSO)/max(1e-3,TE-TS)
        Overlap=c(Overlap,OverlapTemp)
        TS=min(TS,IdSub$Tstart[j])
        TE=max(TE,IdSub$Tend[j])
        
        if(j==1)
        {
          FreqDiff=c(FreqDiff,999)
          FreqRatio=c(FreqRatio,999)
          FreqInit=IdSub$FreqM[j]
        }else{
          FreqDiff=c(FreqDiff,abs(IdSub$FreqM[j]-FreqInit))
          FreqRatio=c(FreqRatio,IdSub$FreqM[j]/FreqInit)
        }
        }
      
      #print(levels(as.factor(IdTot$Group.1))[i])
      #print(i)
      
      
      #print(length(Overlap))
      IdTot2=rbind(IdTot2,IdSub)
      #print(nrow(IdTot2))
      
      #if(i%%10==1)
      #{
       # print(paste(i,nlevels(as.factor(IdTot$Group.1)),Sys.time()))
      #}
      
      
    }
    
    IdTot2$Overlap=Overlap
    IdTot2$FreqDiff=FreqDiff
    IdTot2$FreqRatio=FreqRatio
    
    ColIdTotSp=c(match(SpeciesList$Nesp,colnames(IdTot2))
                 ,match(SpeciesList$Esp,colnames(IdTot2)))
    ColIdTotSpSel=unique(ColIdTotSp[!is.na(ColIdTotSp)])
    IdTotSp=IdTot[,..ColIdTotSpSel]
    
    ProbaMax=aggregate(IdTotSp
                       ,by=list(IdTot$Group.1), FUN=max)
    colnames(ProbaMax)[2:ncol(ProbaMax)]=paste0(colnames(ProbaMax)[2:ncol(ProbaMax)],"_Max")
    
    IdTot_pourRF=merge(IdTot2,ProbaMax,by="Group.1")
    if(nrow(IdTot2)!=nrow(IdTot_pourRF)){stop("problem w merge")}
    
    #print(names(IdTot_pourRF))
    #print(row.names(ClassifNbSp$importance))
    
    NbSp=predict(ClassifNbSp,IdTot_pourRF,type="prob",norm.votes=T)
    
    IdTot_pourRF2=cbind(IdTot_pourRF,NbSp)
    
    ColIdTotSp=c(match(SpeciesList$Nesp,colnames(IdTot_pourRF2))
                 ,match(SpeciesList$Esp,colnames(IdTot_pourRF2)))
    ColIdTotSpSel=unique(ColIdTotSp[!is.na(ColIdTotSp)])
    
    IdTot3=IdTot_pourRF2[0,]
    for (i in 1:nlevels(as.factor(IdTot_pourRF2$Group.1)))
    {
      IdSub=subset(IdTot_pourRF2,IdTot_pourRF2$Group.1==levels(as.factor(IdTot_pourRF2$Group.1))[i])
      AGarder=subset(IdSub,(IdSub$'FALSE'<0.62)|(IdSub$OrderNum==1))
      AFusionner=subset(IdSub,(IdSub$'FALSE'>=0.62)&(IdSub$OrderNum>1))
      ATSp=AFusionner[,..ColIdTotSpSel]
      AGSp=AGarder[,..ColIdTotSpSel]
      if(nrow(AFusionner)>0)
      {
        for (j in 1:nrow(AFusionner))
        {
          DistProb=as.matrix(dist(rbind(ATSp[j,],AGSp)))
          DistMin=as.numeric(which.min(DistProb[(2:nrow(DistProb)),1]))
          for (k in ColIdTotSpSel)
          {
            AGarder[DistMin,k]=max(AGarder[DistMin,..k],AFusionner[j,..k])
          }
        }
      }
      IdTot3=rbind(IdTot3,AGarder)
      
      if(i%%1000==1)
      {
        #print(paste(i,nlevels(as.factor(IdTot_pourRF2$Group.1)),Sys.time()))
      }
      
    }
    
    #suppress useless columns
    ColIdTot=match(colnames(IdTot),colnames(IdTot3))
    ColIdTotSel=unique(ColIdTot[!is.na(ColIdTot)])
    IdTot4=IdTot3[,..ColIdTot]
    
  }else{
    IdTot4=IdTot
  }
  
  #adding version number from both Tadarida-D and Tadarida-C
  IdTot4=cbind(IdTot4,VersionD=CTP$Version[1],VersionC=Version)
  #IdTot4$Order=NULL
  
  #compute real success probability
  RefErrorRisk=fread(args[13])
  IdTot4$SuccessProb=999
  IdTottemp=IdTot4[0,]
  for (i in 1:nlevels(as.factor(IdTot4$SpMaxF2)))
  {
    IdToti=subset(IdTot4,IdTot4$SpMaxF2==levels(as.factor(IdTot4$SpMaxF2))[i])
    Refi=subset(RefErrorRisk,RefErrorRisk$Espece==levels(as.factor(IdTot4$SpMaxF2))[i])
    if((nrow(Refi)==1)&(Refi$Pente[1]>0))
    {
      IdToti$SuccessProb=exp(Refi$Int[1]+Refi$Pente[1]*IdToti$Ind)/(1+exp(Refi$Int[1]+Refi$Pente[1]*IdToti$Ind))
    }else{
      IdToti$SuccessProb=IdToti$Ind
    }
    IdTottemp=rbind(IdTottemp,IdToti)
  }
  
  IdTottemp$SuccessProb=round(IdTottemp$SuccessProb,2)
  IdTottemp$SuccessProb=pmin(IdTottemp$SuccessProb,0.99)
  
  IdTot4=IdTottemp
  
  
  
  if(args[6])
  {
    #print(table(IdTot4$Group.1))
    #writing .tc files
    for (i in 1:nlevels(as.factor(IdTot4$Group.1)))
    {
      fichier=levels(as.factor(IdTot4$Group.1))[i]
      fichierid=paste(tadir,'/',substr(fichier,1,(nchar(fichier)-4)),".tc", sep="")
      write.csv(subset(IdTot4,IdTot4$Group.1==fichier),fichierid,row.names=FALSE)  
    }
  }else{
    fwrite(IdTot4,paste0(tadir,"/IdTri.csv"))
    fwrite(cbind(IdTot4[,1,with=F],IdTot4[,((ncol(IdTot4)-18):ncol(IdTot4)),with=F]),paste0(tadir,"/Idshort.csv"))
  }
  
  #for test
  print(table(IdTot4$SpMaxF2))
  
#}else{
 # print("no sound sequences to aggregate")
#}
