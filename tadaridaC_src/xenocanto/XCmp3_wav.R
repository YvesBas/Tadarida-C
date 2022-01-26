library(tuneR)
DirDest="I:"
print("L3")
#SaveWD=getwd()
#setwd(DirDest)

ListW=list.files(DirDest,pattern=".wav$",full.names=T)
ListM=list.files(DirDest,pattern=".mp3$",full.names=T)

while(length(ListW)<length(ListM)) #to handle quite frequent crashes in mp32wav function
{
  Sys.time()
  for (i in 1:length(ListM))
  {
  if(!gsub(".mp3",".wav",basename(ListM[i])) %in% basename(ListW)){
    r <- readMP3(ListM[i])
    r=normalize(r,unit="16")
    try(writeWave(r,gsub(".mp3",".wav",ListM[i]),extensible=FALSE))
    }
    if(i%%1==0){print(paste(i,length(ListM),ListM[i],Sys.time()))}
    }
    Sys.time()
  ListW=list.files(DirDest,pattern=".wav$",full.names=T)
  ListM=list.files(DirDest,pattern=".mp3$",full.names=T)
  print(paste(length(ListW),length(ListM),sep="/"))
}

