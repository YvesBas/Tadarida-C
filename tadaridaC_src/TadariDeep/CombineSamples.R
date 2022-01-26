library(data.table)

DirSample="./Tadarida/Dbas_deepL/tests_birdnet"
SampleList=c("NocMig_TD13.csv"
             ,"NocMig_TD250.csv"
             ,"NM_BN_S25.csv"
             ,"NM_BN_NW1.csv")

#File=vector()
FileList=list()
for (i in 1:length(SampleList))
{
  FileList[[i]]=fread(paste0(DirSample,"/",SampleList[i]))
#  File=c(File,Samplei$Fsel)
}

SampleAll=rbindlist(FileList)
fwrite(SampleAll,paste0("SampleAll",Sys.Date(),".csv"),sep=";")
