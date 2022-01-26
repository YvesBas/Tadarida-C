library(tuneR)
library(data.table)

RSDB="C:/Users/yvesb/Documents/RSDB_HF"
OutF="C:/Users/yvesb/Documents/RSDB_HF_ExpData.csv"

LW=list.files(RSDB,pattern=".wav$",recursive=T,full.names=T)

EF=vector()
for (i in 1:length(LW))
{
 if(i%%100==1){print(paste(i,Sys.time()))}
   Wi=readWave(LW[i])
  
   FFeaturesi=paste0(dirname(LW[i]),"/txt/"
                           ,gsub(".wav",".ta",basename(LW[i])))
   
  if(file.exists(FFeaturesi)){  
  Featuresi=fread(FFeaturesi)
  
EFi=Featuresi$SampleRate[1]/Wi@samp.rate  
  }else{
    EFi=NA
  }
EF=c(EF,EFi)
}
table(EF)

ExpData=data.frame(File=LW,EF)

fwrite(ExpData,OutF,sep=";")
