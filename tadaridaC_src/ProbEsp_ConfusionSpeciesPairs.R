library(randomForest)

#load("ClassifEsp_tabase3HF_France_Cir_2019-11-26_wiSR.learner")
ProbEsp=fread("ProbEspHF2019-11-26_wiSR.csv")
ProbEsp=fread("ProbEspHF2019-11-28.csv")
#tabaseHF=fread("RSDB_HF_tabase3HF_sansfiltre.csv")
SpeciesList=fread("SpeciesList.csv")

ColProb=subset(colnames(ProbEsp),colnames(ProbEsp) %in% SpeciesList$Esp)

#PETB=merge(ProbEsp,tabaseHF,by=c("Filename","CallNum"))

#PEtest=paste(ProbEsp$Filename,ProbEsp$CallNum)
#TBtest=paste(tabaseHF$Filename,tabaseHF$CallNum)

#table(ProbEsp$Filename,(PEtest %in% TBtest))

PEpippip=subset(ProbEsp,ProbEsp$Espece=="Pippip")
Ppippip=PEpippip[,c(13:83)]
Pmax=apply(Ppippip,MARGIN=1,FUN=max)
plot(PEpippip$Pippip,PEpippip$Pmax)

ScoreTot=data.frame()
for (i in 1:length(unique(ProbEsp$Espece)))
{
  PEi=subset(ProbEsp,ProbEsp$Espece==unique(ProbEsp$Espece)[i])
  PEPi=subset(PEi,select=ColProb)
  #PEPiScores=apply(PEPi,MARGIN=2,FUN=function(x) max(x,na.rm = T))
  #PEPiScores=PEPiScores[order(PEPiScores,decreasing=T)]
  #head(names(PEPiScores),20)
  PEPiScores=apply(PEPi,MARGIN=2,FUN=function(x) mean(x,na.rm = T))
  PEPiScores=PEPiScores[order(PEPiScores,decreasing=T)]
  #head(names(PEPiScores),20)
  
  
  Scorei=data.frame(Species=unique(ProbEsp$Espece)[i]
                    ,SpeciesId=names(PEPiScores),Score=PEPiScores)
ScoreTot=rbind(ScoreTot,Scorei)
    #head(names(PEPiScores),10)
}
fwrite(ScoreTot,"ConfsionSpeciesPairs.csv",sep=";")
