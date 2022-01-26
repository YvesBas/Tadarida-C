library(tuneR)

DirW="C:/Users/yvesb/Documents/Tadarida/pollinisateurs/doc_voix_humaine/voix_humaine_database"

ListWrecent=list.files(DirW,pattern=".wav$",full.names=T)
WNrates=c(10,24,50,90)
CoeffSR=1


for (i in 1:length(WNrates))
{
  
DestMix=paste0(DirW,"/wn",WNrates[i])
dir.create(DestMix)

for (j in 1:length(ListWrecent))
{
  Dur=0
  if((file.size(ListWrecent[j]))>50000)
  {
    tempW=readWave(ListWrecent[j])
    Dur=length(tempW@left)/tempW@samp.rate
    if(Dur>0)
    {
      wn=noise(kind="white",duration=length(tempW@left),samp.rate=tempW@samp.rate
               ,bit=tempW@bit,pcm=tempW@pcm)
      Mix=tempW*(1-WNrates[i]/100)+wn*WNrates[i]/100
      Mix=normalize(Mix,unit="16")
      Mix@samp.rate=Mix@samp.rate/CoeffSR
      writeWave(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
      
        #savewav(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
        }
  }
  print(paste(j,ListWrecent[j],Dur))
  }
  
  

}
