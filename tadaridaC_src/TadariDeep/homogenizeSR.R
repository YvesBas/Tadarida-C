library(data.table)
library(tuneR)
library(seewave)
#library(stringr)

RSDB="c:/Users/yvesb/Documents/RSDB_HF/20190829"
DirResampl="G:/testResamp48"
MinSRs = 48000
DownOnly=F
TreatedTadaridaD=F

ListWav=list.files(RSDB,full.names=T,recursive=T,pattern=".wav$")
dir.create(DirResampl)

print(MinSRs)
llw = length(ListWav)
print(llw)

for (i in 1:llw)
{
  print(i)
  print(ListWav[i])
  WavA=readWave(ListWav[i]) #lit le fichier wave
  if(TreatedTadaridaD)
  {
    Features=fread(paste0(dirname(ListWav[i]),"/txt/",gsub(".wav",".ta",basename(ListWav[i]))))
    if(nrow(Features)>0){
      if(Features$SampleRate[1]==WavA@samp.rate){
        SRi=MinSRs
        print("direct")
      }else{
        SRi=MinSRs/10
        print("expansion")
      }
    }
    
  }else{
    SRi=MinSRs
    
  }
  print(WavA@samp.rate)
  if((WavA@samp.rate > SRi)|(!DownOnly))
  {
    WavL=fir(WavA,to=SRi/2,output="Wave") #filtre passe-bas pour éviter l'aliasing
    WavRS=resamp(wave=WavL,g=SRi,output="Wave") #downsampling
    #write(ListWav[i],file="c:/testr/liste1.txt",append=TRUE)
    try(
      {
        #writeWave(normalize(WavRS,unit="16"),filename=gsub(RSDB,DirResampl,ListWav[i]))		
        fwav2 = gsub(RSDB,DirResampl,ListWav[i])
        dn2 = dirname(fwav2)
        if (!file.exists(dn2)){
          dir.create(dn2)
        }
        writeWave(normalize(WavRS,unit="16"),filename=fwav2,extensible=F)		
        
        #write(ListWav[i],file="c:/testr/liste2.txt",append=TRUE)
      }
    )
  }  
  else
  {
    print("non modifie")
    #write(ListWav[i],file="c:/testr/liste3.txt",append=TRUE)
  }
}




