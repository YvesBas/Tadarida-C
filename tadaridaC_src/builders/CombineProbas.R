library(boot)
library(data.table)
args="ProbEspC3_2019-03-25.csv"
SpeciesList=fread("SpeciesList.csv")
GroupingSp=T
Suffixes=c("","_prev")
Weight=c(7,3)

ProbEsp=fread(args)
ProbEsp0=ProbEsp
ListSp=vector()
ColSp=vector()
Suff=vector()
for (i in 1:length(Suffixes))
{
  ColSel=subset(colnames(ProbEsp0)
                ,!(colnames(ProbEsp0) 
                   %in% paste0(SpeciesList$Esp,Suffixes[i])))
  ColSpSupp=subset(colnames(ProbEsp),(colnames(ProbEsp) 
                                      %in% paste0(SpeciesList$Esp,Suffixes[i])))
  ColSp=c(ColSp,ColSpSupp)
  SpSupp=gsub(Suffixes[i],"",ColSpSupp)
  ListSp=c(ListSp,SpSupp)
  Suff=c(Suff,rep(Suffixes[i],length(SpSupp)))
  ProbEsp0=subset(ProbEsp0,select=ColSel)
}


NespMatch=match(ListSp,SpeciesList$Esp)

if(GroupingSp)
{
  SpGroup=SpeciesList$Nesp2[NespMatch]
}else{
  SpGroup=SpeciesList$Nesp[NespMatch]  
}


#define the species list
for (i in 1:nlevels(as.factor(SpGroup)))
{
  ProbaGroup=rep(0,nrow(ProbEsp0))
  for (j in 1:length(Suffixes))
  {
    ScoreSp=subset(ProbEsp
                   ,select=subset(ColSp
                                  ,((SpGroup==levels(as.factor(SpGroup))[i])
                                    &(Suff==Suffixes[j]))))
    if(ncol(ScoreSp)>0)
      {
      ProbaGroup=ProbaGroup+apply(ScoreSp,MARGIN=1,sum)*Weight[j]
    }
    
  }
  ProbaGroup=ProbaGroup/sum(Weight)
  ProbEsp0=cbind(ProbEsp0,ProbaGroup)
  colnames(ProbEsp0)[ncol(ProbEsp0)]=levels(as.factor(SpGroup))[i]
  print(levels(as.factor(SpGroup))[i])
}

fwrite(ProbEsp0,paste0(substr(args[1],1,nchar(args[1])-4)
                       ,"_",ifelse(GroupingSp,"G","")
                       ,Weight[1],".csv"))
