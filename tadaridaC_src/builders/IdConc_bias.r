library(data.table)
#FIdConc="tabase3HF_France_IdConc"
FIdConc="RSDB_HF_tabase3HF_sansfiltre_IdTot_wiSR_IdConc"
GroupingSp=T
SpeciesList=fread("SpeciesList.csv")
DiscardRSDB=F
ColD="Group.1" #indicate where names of data identifier
FiltPrefix="Cir"
FiltChannel="Dir"


IdConc=fread(paste0(FIdConc,".csv"))
testD=match(ColD,colnames(IdConc))

NamesTemp=subset(IdConc,select=ColD)
IdConc$year=tstrsplit(as.data.frame(NamesTemp)[,1],"-")[[2]]
barplot(table(IdConc$year))
IdConc=subset(IdConc,IdConc$year!="")

if(DiscardRSDB)
{
  Sys.time()
  FilesRSDB=list.files("./RSDB_HF",recursive=T
                       ,full.names=T,pattern=".wav$")
  Sys.time()
  IdConc=subset(IdConc,!(as.data.frame(IdConc)[,testD] %in% basename(FilesRSDB)))
}

if(!is.na(FiltPrefix))
{
  NamesTemp=subset(IdConc,select=ColD)
  IdConc=subset(IdConc
                ,substr(as.data.frame(NamesTemp)[,1],1
                        ,nchar(FiltPrefix))==FiltPrefix)
}




if(sum(grepl("IdMan",names(IdConc)))==0)
{
  IdConc$IdMan=IdConc$ValidId
}
#else{
#   IdConc$IdMan=IdConc$valid.espece
#}

if(GroupingSp)
{
  test=match(IdConc$IdMan,SpeciesList$Esp)
  IdConc$IdMan=SpeciesList$Nesp2[test]
}


if(!is.na(FiltChannel))
{
  NamesTemp=subset(IdConc,select=ColD)
GroupTemp=substr(as.data.frame(NamesTemp)[,1],1,34)
  MaxTE=aggregate(IdConc$Tend,by=list(GroupTemp),max)
  hist(MaxTE$x)
  GroupDir=subset(MaxTE$Group.1,MaxTE$x>0.5)
  if(FiltChannel=="Exp")
  {
    IdConc=subset(IdConc,!(GroupTemp %in% GroupDir))
  }else{
    IdConc=subset(IdConc,(GroupTemp %in% GroupDir))
  }
  
}


ListSp=levels(as.factor(IdConc$IdMan))
ListSp=subset(ListSp,ListSp!="")


Sp=vector()
coef1=vector()
p1=vector()
coef2=vector()
p2=vector()
for (i in 1:length(ListSp))
{
  Label=(IdConc$IdMan==ListSp[i])
  DataSp=subset(IdConc,Label)
  barplot(table(DataSp$year),main=ListSp[i])
  testSp=match(ListSp[i],names(DataSp))
  if(!is.na(testSp))
  {
    ScoreSp=DataSp[,..testSp]
    ScoreSp[ScoreSp>1]=1
    msp=glm(as.data.frame(ScoreSp)[,1]~as.numeric(DataSp$year),family="binomial")
    plot(as.numeric(DataSp$year),predict(msp,type="response"),main=ListSp[i])
    Sp=c(Sp,ListSp[i])
    coef1=c(coef1,msp$coefficients[2])
    if(is.na(msp$coefficients[2]))
    {
     p1=c(p1,NA) 
    }else{
    p1=c(p1,summary(msp)$coefficients[2,4])
    }
    DataSp$Err=(DataSp$IdMan!=DataSp$SpMaxF2)
    summary(DataSp$Err)
    mbin=glm(DataSp$Err~as.numeric(DataSp$year),family="binomial")
    plot(as.numeric(DataSp$year),predict(mbin,type="response"),main=ListSp[i])
    coef2=c(coef2,mbin$coefficients[2])
    if(is.na(mbin$coefficients[2]))
    {
      p2=c(p2,NA)
    }else{
      p2=c(p2,summary(mbin)$coefficients[2,4])
    }
  }
}
Output=data.frame(Sp,coef1,p1,coef2,p2)
fwrite(Output,paste0("BiasYear_",FIdConc,"_"
                     ,FiltPrefix,"_",FiltChannel,".csv"),sep=";")
 