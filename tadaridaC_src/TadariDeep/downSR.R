library(data.table)
library(tuneR)
library(seewave)
#library(stringr)

RSDB="./www/wav_sample_crepurb25c" #dossier où sont les waves à homogénéiser
DirResampl="./www/wav_downSampleC" #dossier de sortie
NewSampleRate=48000


ListWav=list.files(RSDB,full.names=T,recursive=T,pattern=".wav$")
dir.create(DirResampl)

SRs=vector()
for (i in 1:length(ListWav))
{
  WavA=readWave(ListWav[i]) #lit le fichier wave
  print(paste(WavA@samp.rate,basename(ListWav[i])))
  SRs=c(SRs,WavA@samp.rate) #lit son taux d'échantilonnage
  WavL=fir(WavA,to=NewSampleRate/2,output="Wave") #filtre passe-bas pour éviter l'aliasing
  if(WavA@samp.rate > NewSampleRate)
  {
    WavRS=resamp(wave=WavL,g=NewSampleRate,output="Wave") #downsampling
    #write(ListWav[i],file="c:/testr/liste1.txt",append=TRUE)
    try(
      {
        writeWave(normalize(WavRS,unit="16"),filename=gsub(RSDB,DirResampl,ListWav[i])
                  ,extensible=F) #ecrit le nouveau fichier wave
        #write(ListWav[i],file="c:/testr/liste2.txt",append=TRUE)
      }
    )
    
  }else{
    writeWave(normalize(WavA,unit="16"),filename=gsub(RSDB,DirResampl,ListWav[i])
              ,extensible=F) #ecrit le nouveau fichier wave
  }
}
