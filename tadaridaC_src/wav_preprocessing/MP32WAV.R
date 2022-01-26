library(bioacoustics)
library(tuneR)

DirMP3="C:/Users/yvesb/Documents/Tadarida/pollinisateurs/doc_voix_humaine/voix_humaine_database"

FileMP3=list.files(DirMP3,full.names=T,pattern=".mp3$")

for (i in 1:length(FileMP3))
{
  #r <- readMP3(FileMP3[i])  
mp3_to_wav(FileMP3[i])
}


##plus robuste ?
##library(tuneR)
##r <- readMP3("04 Trip to Paris.mp3")  ## MP3 file in working directory
#writeWave(r,"tmp.wav",extensible=FALSE)
