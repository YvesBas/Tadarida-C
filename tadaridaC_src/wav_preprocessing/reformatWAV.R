library(tuneR)

DirToTreat="C:/Users/yvesb/Documents/www/wav de jeutest10183"

ListWrecent=list.files(DirToTreat,pattern=".wav$",full.names=T)
for (j in 1:length(ListWrecent))
{
  Dur=0
  if((file.size(ListWrecent[j]))>50000)
  {
    tempW=readWave(ListWrecent[j])
    Dur=length(tempW@left)/tempW@samp.rate
    if(Dur>0)
    {
      #tempW@samp.rate=tempW@samp.rate*10
      #Mix=normalize(Mix,unit="16")
      writeWave(normalize(tempW,unit="16"),ListWrecent[j],extensible=F)
      
      #savewav(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
    }
  }
  print(paste(j,ListWrecent[j],Dur))
}

