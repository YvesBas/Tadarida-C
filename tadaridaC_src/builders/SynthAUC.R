AUCgraphDir="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point_du_5_janvier_2022/tests_FileAUC"
filterNmin=10 #échantillon minimum pour tenir compte d'une espèce
ClassCompl=c("noise","Otherbirds","Turili","Turphi","Erirub","Nycnyc","Grugru","Stralu")


AUCfiles=list.files(AUCgraphDir,pattern="_AUC.csv$",full.names=T)

AllData=list()
for (i in 1:length(AUCfiles))
{
  filei=fread(AUCfiles[i])
  Classif1=tstrsplit(basename(AUCfiles[i]),split="_")[[2]]
  ClassifE=tstrsplit(basename(AUCfiles[i]),split="_")[[3]]
  filei$Classif=paste0(Classif1,"_",ClassifE)
  AllData[[i]]=filei
}
AllDataAUc=rbindlist(AllData)

AllDataAUcF=subset(AllDataAUc,AllDataAUc$N>filterNmin)
table(AllDataAUcF$Espece)

SynthClassif=aggregate(AllDataAUcF$AUC
                       ,by=list(AllDataAUcF$Classif),mean)

names(SynthClassif)=c("classifieur","AUCmean")
if(length(ClassCompl)>0){
  
for (j in 1:length(ClassCompl))
{
Compl=subset(AllDataAUcF,AllDataAUcF$Espece==ClassCompl[j])
Compl=Compl[order(Compl$Classif)]
SynthClassif$Compl=Compl$AUC
names(SynthClassif)[ncol(SynthClassif)]=ClassCompl[j]
}
}

fwrite(SynthClassif,paste0(AUCgraphDir,"/SynthClassif.csv"),sep=";")
  
       
