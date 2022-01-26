library(randomForest)

load("ClassifEspC3_2019-03-25.learner")
ProbEsp=fread("ProbEspC3_2019-03-25_G7.csv")
GroupFilter="bush-cricket"
SpeciesList=fread("SpeciesList.csv")
RefSeuils=fread("Referentiel_seuils_ProbEspC3_2019-03-25_G7__D_G.csv")

summary(confusion.matrix(ClassifEspC2b))
print(ClassifEspC2b)

test=match(ClassifEspC2b$predicted,SpeciesList$Esp)
ClassifEspC2b$predictedG=SpeciesList$Nesp2[test]
test=match(ClassifEspC2b$y,SpeciesList$Esp)
ClassifEspC2b$yG=SpeciesList$Nesp2[test]

SumVotes=apply(ClassifEspC2b$votes,MARGIN=1,sum)
MaxVotes=apply(ClassifEspC2b$votes,MARGIN=1,max)
ClassifEspC2b$Score=MaxVotes/SumVotes


FPrate=vector()
Species=vector()
for (i in 1:length(unique(ClassifEspC2b$predictedG)))
{
  NumCol=match(unique(ClassifEspC2b$predictedG)[i],colnames(ProbEsp))
  Seuil=subset(RefSeuils$Seuil50,RefSeuils$Espece==unique(ClassifEspC2b$predictedG)[i])
  if(length(Seuil)>0)
  {
    Predi=subset(ClassifEspC2b$yG
                 ,(ClassifEspC2b$predictedG==unique(ClassifEspC2b$predictedG)[i])&
                   ProbEsp[,..NumCol]>Seuil)
    if(length(Predi)>0){
      FPrate=c(FPrate,mean(1-(Predi==unique(ClassifEspC2b$predictedG)[i])))
      Species=c(Species,unique(ClassifEspC2b$predictedG)[i])
    }}
}
DataFP=data.frame(Species,FPrate)
DataFP=DataFP[order(DataFP$Species),]
DataFPs=merge(DataFP,SpeciesList,by.x="Species",by.y="Esp")
DataGroup=subset(DataFPs,DataFPs$Group==GroupFilter)
fwrite(DataGroup,"DataGroupFP.csv",sep=";")

