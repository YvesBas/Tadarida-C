library(tuneR)

DirW="C:/Users/yvesb/Documents/Tadarida/pollinisateurs/doc_voix_humaine/voix_humaine_database" #input folder
DestMix="C:/Users/yvesb/Documents/Tadarida/pollinisateurs/doc_voix_humaine/voix_humaine_database_noisedCoef40" #output folder
CoeffSR=0.4 #change in sampling rate
WNrange=c(0,90) #range of noising proportion (integers between 0 and 100)

ListWrecent=list.files(DirW,pattern=".wav$",full.names=T)
dir.create(DestMix)
#WNrates=c(10,24,50,90)

#for (i in 1:length(WNrates))
#{
WNpotentials=c(WNrange[1]:WNrange[2])

for (j in 1:length(ListWrecent))
{
  Dur=0
  if((file.size(ListWrecent[j]))>50000)
  {
    
    WNrate=sample(WNpotentials,1)
    tempW=readWave(ListWrecent[j])
    Dur=length(tempW@left)/tempW@samp.rate
    if(Dur>0)
    {
      wn=noise(kind="white",duration=length(tempW@left),samp.rate=tempW@samp.rate
               ,bit=tempW@bit,pcm=tempW@pcm)
      Mix=tempW*(1-WNrate/100)+wn*WNrate/100
      Mix=normalize(Mix,unit="16")
      Mix@samp.rate=Mix@samp.rate/CoeffSR
      writeWave(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
      
      #savewav(Mix,filename=paste0(DestMix,"/",basename(ListWrecent[j])))
    }
  }
  print(paste(j,ListWrecent[j],Dur))
}




