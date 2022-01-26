library(tuneR)

DirToTreat=dir("mnt/XC",pattern="wn",full.names=T)
DirToTreat=subset(DirToTreat,substr(basename(DirToTreat),1,2)=="wn")


for (i in 1:length(DirToTreat))
{
 ListWrecent=list.files(DirToTreat[i],pattern=".wav$",full.names=T)
 ListWrecent=subset(ListWrecent,!grepl("-x10.wav",ListWrecent))
   for (j in 1:length(ListWrecent))
  {
    Dur=0
    if((file.size(ListWrecent[j]))>50000)
    {
      tempW=readWave(ListWrecent[j])
      Dur=length(tempW@left)/tempW@samp.rate
      if(Dur>0)
      {
        tempW@samp.rate=tempW@samp.rate*10
        #Mix=normalize(Mix,unit="16")
        writeWave(tempW,gsub(".wav","-x10.wav",ListWrecent[j]),extensible=F)
        
        #savewav(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
      }
    }
    print(paste(j,ListWrecent[j],Dur))
   }
}
