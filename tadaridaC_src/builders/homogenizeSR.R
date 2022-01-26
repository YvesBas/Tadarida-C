library(data.table)
library(tuneR)
library(seewave)
#library(stringr)

RSDB="./Tadarida/Dbas_deepL/point du 28-06-2021/wav de birdnet 13 esp traites tadarida" #dossier où sont les waves à homogénéiser
DirResampl="./testResample" #dossier de sortie

ListWav=list.files(RSDB,full.names=T,recursive=T,pattern=".wav$")
dir.create(DirResampl)

SRs=vector()
for (i in 1:length(ListWav))
{
  WavA=readWave(ListWav[i]) #lit le fichier wave
  #print(WavA@samp.rate)
  SRs=c(SRs,WavA@samp.rate) #lit son taux d'échantilonnage
  #writeWave(WavAugmente,filename=paste0(DirAugmentes,"/WavAugmente_",NumbPad,".wav")) #ecrit le nouveau fichier wave
   }
}

MinSRs=min(SRs) #calcul le taux d'échantillonnage minimum
print(MinSRs)

for (i in 1:length(ListWav))
{
  WavA=readWave(ListWav[i]) #lit le fichier wave
  print(WavA@samp.rate)
  WavL=fir(WavA,to=MinSRs/2,output="Wave") #filtre passe-bas pour éviter l'aliasing
  WavRS=resamp(wave=WavL,g=MinSRs,output="Wave") #downsampling
    writeWave(WavRS,filename=gsub(RSDB,DirResampl,ListWav[i])) #ecrit le nouveau fichier wave
}
}




