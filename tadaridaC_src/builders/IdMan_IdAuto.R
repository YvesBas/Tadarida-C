library(data.table)

#args="tabase3HF_1015France_IdMan.csv"
args="IdExhaustifs.csv"
args[2]="C:/wamp64/www/Exhaustifs/txt1/IdTot.csv"
args[3]="SpeciesList.csv"
args[4]=T #exhaustive? (mean every species within the files are identified so all IdAuto should have a IdMan match)


IdMan=fread(args[1])
colnames(IdMan)[10]="temps_fin"

test=match("Group.1",colnames(IdMan))

if(is.na(test))
{
  test=match("donnee",colnames(IdMan))
  if(is.na(test))
  {
    IdMan$Group.1=IdMan$'nom du fichier'
  }else{
    IdMan$Group.1=IdMan$donnee
  }
}

IdAuto=fread(args[2])
SpeciesList=fread(args[3])

test=match("valid.espece",colnames(IdMan))
if(is.na(test))
{
  IdManG=merge(IdMan,SpeciesList,by.x="Group.2",by.y="Esp",all.x=T)
  
}else{
  IdManG=merge(IdMan,SpeciesList,by.x="valid.espece",by.y="Esp",all.x=T)
}
(nrow(IdManG)==nrow(IdMan))
test=subset(IdManG,is.na(IdManG$Nesp))
table(test$Group.2)
table(test$valid.espece)

#check consistency between the two tables
if(substr(IdManG$Group.1[1]
          ,nchar(IdManG$Group.1[1])-3,nchar(IdManG$Group.1[1]))==".wav")
{
  IdManG$Group.1=substr(IdManG$Group.1,1
                        ,nchar(IdManG$Group.1)-4)
}

if(substr(IdAuto$Group.1[1]
          ,nchar(IdAuto$Group.1[1])-3,nchar(IdAuto$Group.1[1]))==".wav")
{
  IdAuto$Group.1=substr(IdAuto$Group.1,1
                        ,nchar(IdAuto$Group.1)-4)
}

IdManG=subset(IdManG,IdManG$Group.1 %in% levels(as.factor
                                                (IdAuto$Group.1)))

IdAuto=subset(IdAuto,IdAuto$Group.1 %in% levels(as.factor
                                                (IdManG$Group.1)))


IdAuto$IdMan=""
IdAuto$participation=""

IdConc=IdAuto[0,]
for (i in 1:nlevels(as.factor(IdAuto$Group.1))) #0.02 sec / données
{
  IdSubA=subset(IdAuto,IdAuto$Group.1==levels(as.factor(IdAuto$Group.1))[i])
  IdSubM=subset(IdManG,IdManG$Group.1==levels(as.factor(IdAuto$Group.1))[i])
  test1=match(IdSubA$SpMaxF2,IdSubM$Nesp)
  test2=match(IdSubM$Nesp,IdSubA$SpMaxF2)
  EspRates=subset(IdSubM$Nesp,is.na(test2))
  for (j in 1:nrow(IdSubA))
  {
    #print(j)
    if(is.na(test1[j]))
    {
      #print("cas47")
      if(length(EspRates)>0)
      {
        #print("cas51")
        ScoreEsp=vector()
        for (k in 1:length(EspRates))
        {
          test=match(EspRates[k],colnames(IdSubA))
          if(is.na(test))
          {
            ScoreEsp=c(ScoreEsp,0)
          }else{
            ScoreEsp=c(ScoreEsp,IdSubA[j,..test])
          }
        }
        ScoreEsp=unlist(ScoreEsp)
        test=match(max(ScoreEsp),ScoreEsp)
        IdSubA$IdMan[j]=EspRates[test]
        EspRates=subset(EspRates,EspRates!=EspRates[test])
      }else{
        #print("cas68")
        ScoreEsp=vector()
        for (k in 1:nrow(IdSubM))
        {
          test=match(IdSubM$Nesp[k],colnames(IdSubA))
          if(is.na(test))
          {
            ScoreEsp=c(ScoreEsp,0)
          }else{
            ScoreEsp=c(ScoreEsp,IdSubA[j,..test])
          }
        }
        #print("cas76")
        ScoreEsp=unlist(ScoreEsp)
        test=match(max(ScoreEsp),ScoreEsp)
        #print(test)
        #print(IdSubM$Nesp)
        EspTemp=IdSubM$Nesp[test]
        IdSubA$IdMan[j]=EspTemp
      }
      
    }else{
      
      
      IdSubA$IdMan[j]=IdSubM$Nesp[test1[j]]
      
    }
  }
  IdSubA$participation=IdSubM$participation[1]
  IdConc=rbind(IdConc,IdSubA)
  if(i%%1000==1)
  {
    print(paste(i,nlevels(as.factor(IdAuto$Group.1)),Sys.time()))
  }
}

ColIdCSp=match(SpeciesList$Esp,colnames(IdConc))
ColIdCSpSel=unique(ColIdCSp[!is.na(ColIdCSp)])
IdCSp=IdConc[,..ColIdCSpSel]


IdConc$Indice=apply(IdCSp,MARGIN=1,FUN=max)

fwrite(IdConc,paste0(substr(args[1],1,nchar(args[1])-10),"_IdConc.csv"))
