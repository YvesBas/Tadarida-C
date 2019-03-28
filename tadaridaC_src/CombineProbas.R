library(data.table)
args="ProbEspC3_2019-03-25_G7.csv"
SpeciesList=fread("SpeciesList.csv")
Ref=fread("Referentiel_seuils_ProbEspC3_2019-03-25_G7__D_G.csv")

ProbEsp=fread(args)

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
SpMax2=apply(Probas,MARGIN=1,function(x) which.max(x))
ProbEsp0$SpMax2=colnames(Probas)[SpMax2]

VotesDiverging=subset(ProbEsp0,ProbEsp0$SpMax1!=ProbEsp0$SpMax2)
test=sample.int(nrow(VotesDiverging),1)
VotesDiverging$Group.1[test]
VotesDiverging$Ind[test]
VotesDiverging$Duree[test]
VotesDiverging$FreqC[test]
VotesDiverging$SpMaxF2[test]
VotesDiverging$SpMax1[test]
VotesDiverging$SpMax2[test]
VotesDiverging$ValidId[test]

