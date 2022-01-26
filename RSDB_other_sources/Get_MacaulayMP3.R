library(data.table)


DirMac="https://macaulaylibrary.org/asset/"
PrefAdr="https://cdn.download.ams.birds.cornell.edu/api/v1/asset/"
SuffAdr="/audio"
#FileList=fread("C:/Users/yvesb/Downloads/ML_2022-01-08T11-39_audio_FR.csv")
OutDir="D:/Macaulay/all"
Metadata="C:/Users/yvesb/Documents/Tadarida/Macaulay/Metadata"


MDfiles=list.files(Metadata,full.names=T)

MDlist=list()
for (h in 1:length(MDfiles))
{
  MDlist[[h]]=fread(MDfiles[h])
  MDlist[[h]]$Date=as.character(MDlist[[h]]$Date)
}
FileList=rbindlist(MDlist,use.names=T,fill=T)

dir.create(OutDir)

for (i in 1:nrow(FileList))
{
  if(i%%100==1){
  #Adressi=paste0(DirMac,"/",FileList$`ML Catalog Number`[i])
  #Htmli=try(read.table(Adressi,sep="$")) #dirty
  #fwrite(Htmli,"test.csv")
  print(paste(i,"/",nrow(FileList),Sys.time()))
  print(FileList$`Scientific Name`[i])
  }
  Adressi=paste0(PrefAdr,FileList$`ML Catalog Number`[i],SuffAdr)
  NameFile=paste0(OutDir,"/",FileList$`ML Catalog Number`[i],".mp3")
  download.file(Adressi,NameFile, quiet = TRUE, mode = "wb")
}

fwrite(FileList,"MacaulayAllEurope.csv",sep=";")

