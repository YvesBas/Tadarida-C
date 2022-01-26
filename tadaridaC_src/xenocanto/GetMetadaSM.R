#library(xml2)
library(data.table)
Year="2016"
Site="https://sonotheque.mnhn.fr/sounds/mnhn/so"
DownloadPath="https://sonotheque.mnhn.fr/download/sounds/"
OutDir="C:/Users/yvesb/Documents/test"

dir.create(OutDir)

for (i in 1:10000)
{
  #Htmli=read_html(paste0(Site,"/",Year,"-",i))
  Htmli=read.table(paste0(Site,"/",Year,"-",i),sep="$") #dirty

  test=subset(Htmli,grepl("1467999143302yAWF7ZQpAIKPhP0I",Htmli$V1))
  GetWavLine=subset(Htmli,grepl("waveform",Htmli$V1))
  Idi=tstrsplit(GetWavLine$V1[2],split="/")[[3]]
  DownLoadi=paste0(DownloadPath,Idi)
  GetTitleLine=subset(Htmli,grepl("<title>",Htmli$V1))
  Titlei=tstrsplit(GetTitleLine$V1[1],split="title>")[[2]]
  Titlei=gsub("</","",Titlei)
  
  
  
  NameFile=paste0(OutDir,"/",Titlei,".wav")
  
  test=try(download.file(DownLoadi,NameFile, quiet = TRUE
                         , mode = "wb"))
  }
