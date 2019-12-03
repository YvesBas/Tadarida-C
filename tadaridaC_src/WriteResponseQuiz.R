library(data.table)
library(xlsx)
DirQuiz="./chiros/quizRLC_groupes"
#DirQuiz="./chiros/quizRLC_mix"

Tabase=fread("tabase3HF_France.csv")
CodesConfiance=c("POSSIBLE","PROBABLE","SUR","SUR","SUR")


ListQuiz=list.dirs(DirQuiz,recursive=F)

for (h in 1:length(ListQuiz))
{
  print(h)
  
  ListW=list.files(ListQuiz[h],pattern=".wav$")
  FSel=basename(ListW)
  Ids=vector()
  Lieu=vector()
  Auteur=vector()
  for (i in 1:length(ListW))
  {
    Tsub=subset(Tabase,Tabase$Filename==FSel[i])
    Tagg=aggregate(Tsub$Indice,by=list(Tsub$Espece),max)
    Identif=""
    for (j in 1:nrow(Tagg))
    {
      Identif=paste0(Identif,Tagg$Group.1[j]," ")
      Conf=CodesConfiance[Tagg$x[j]]
      Identif=paste0(Identif,Conf)
      if(j<nrow(Tagg)){Identif=paste0(Identif,", ")}
    }
    Ids=c(Ids,Identif)
    Lieu=c(Lieu,paste0(Tsub$Site[1],", ",Tsub$Zone[1]))
    Auteur=c(Auteur,Tsub$Auteur[1])
  }
  TabQuiz=data.frame(Fichier=FSel,Ids,Lieu,Auteur)
  TabQuizWoR=TabQuiz
  TabQuizWoR$Ids=NULL

  write.xlsx(TabQuizWoR
             , file=paste0(ListQuiz[h],"/0_InfosFichiers_"
                           ,basename(ListQuiz[h]),".xlsx")
             , sheetName="sans_reponses", row.names=FALSE)
  write.xlsx(TabQuiz
             , file=paste0(ListQuiz[h],"/0_InfosFichiers_"
                           ,basename(ListQuiz[h]),".xlsx")
             , sheetName="avec_reponses", append=TRUE, row.names=FALSE)
      
}
