Test=F
Rescale_Probas=function(probas,splist,ref,minp,maxp,accuracy)
{
  library(data.table)
  print("L5")
  SpeciesList=fread(splist)
  print("L6")
  Ref=read.csv2(ref)
  print("L9")
  Ref$Pente=as.numeric(as.character(Ref$Pente))
  Ref$Int=as.numeric(as.character(Ref$Int))
  
  
  ProbEsp=probas
  
  SelCol=subset(colnames(ProbEsp),!(colnames(ProbEsp) %in% SpeciesList$Esp))
  ProbEsp0=subset(ProbEsp,select=SelCol)
  ListSpPE=subset(colnames(ProbEsp),(colnames(ProbEsp) %in% SpeciesList$Esp))
  for (i in 1:length(ListSpPE))
  {
    Refi=subset(Ref,Espece==ListSpPE[i])
    Scorei=subset(ProbEsp,select=ListSpPE[i])
    if((nrow(Refi)==1)&(Refi$Pente[1]>0))
    {
      Probi=exp(Refi$Int[1]+Refi$Pente[1]*Scorei[,1])/(1+exp(Refi$Int[1]+Refi$Pente[1]*Scorei[,1]))
    }else{
      Probi=Scorei[,1]
    }
    Probi=sapply(Probi,function(x) pmin(maxp,(pmax(minp,x))))
    Probi=round(Probi,accuracy)
    ProbEsp0=cbind(ProbEsp0,Probi)
    colnames(ProbEsp0)[ncol(ProbEsp0)]=ListSpPE[i]
    print(ListSpPE[i])
  }
  
  test=match(colnames(ProbEsp),SpeciesList$Esp)
  Probas=subset(ProbEsp,select=subset(colnames(ProbEsp),!is.na(test)))
  SpMax1=apply(Probas,MARGIN=1,function(x) which.max(x))
  ProbEsp0$SpMax1=colnames(Probas)[SpMax1]
  
  test=match(colnames(ProbEsp0),SpeciesList$Esp)
  Probas=subset(ProbEsp0,select=subset(colnames(ProbEsp0),!is.na(test)))
  Score=apply(Probas,MARGIN=1,max)
  ProbEsp0=ProbEsp0[order(Score,decreasing=T),]
  ProbEsp0=ProbEsp0[order(ProbEsp0$Group.1),]
  
  test=match(colnames(ProbEsp0),SpeciesList$Esp)
  Probas=subset(ProbEsp0,select=subset(colnames(ProbEsp0),!is.na(test)))
  Probas=as.data.frame(Probas)
  
  Sys.time()
  SpMax2=vector()
  PrevFile=""
  SpPrev=vector()
  for (i in 1:nrow(Probas))
  {
    #if(i%%10000==1){print(paste(i,Sys.time()))}
    if(PrevFile!=ProbEsp0$Group.1[i]){SpPrev=vector()}
    if(length(SpPrev)>0)
    {
      for (j in 1:length(SpPrev))
      {
        SelCol=match(SpPrev[j],colnames(Probas))
        Probas[i,SelCol]=0
        }
    }
    SpSel=colnames(Probas)[which.max(Probas[i,])]
    SpMax2=c(SpMax2,SpSel)
    SpPrev=c(SpPrev,SpSel)
    PrevFile=ProbEsp0$Group.1[i]
  }
  Sys.time()
  
  ProbEsp0wP=subset(ProbEsp0,select=subset(colnames(ProbEsp0),is.na(test)))
  ProbEsp0=cbind(ProbEsp0wP,Probas)
  
  ProbEsp0$SpMax2=SpMax2
  
  ProbEsp0
  
  
  #VotesDiverging=subset(ProbEsp0,ProbEsp0$SpMax1!=ProbEsp0$SpMax2)
  #test=sample.int(nrow(VotesDiverging),1)
  #VotesDiverging$Group.1[test]
  #VotesDiverging$Ind[test]
  #VotesDiverging$Duree[test]
  #VotesDiverging$FreqC[test]
  #VotesDiverging$SpMaxF2[test]
  #VotesDiverging$SpMax1[test]
  #VotesDiverging$SpMax2[test]
  #VotesDiverging$ValidId[test]
  
}

if(Test)
{
  ProbEsp=fread("ProbEspC3_2019-03-25.csv")
  Rescale_Probas(probas=ProbEsp,splist="SpeciesList.csv"
                 ,ref="Referentiel_seuils_ProbEspC3_2019-03-25__D_G.csv"
                 ,minp=0.01,maxp=0.99,accuracy=2)
  
}