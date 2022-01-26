library(tuneR)

DirW="./Tadarida/Dbas_deepL/reste_lpo_debut"
DirO="./Tadarida/Dbas_deepL/reste_lpo_debut_std"

dir.create(DirO)

LW=list.files(DirW,pattern=".wav$",full.names = T)

for (i in 1:length(LW)){
  WavRS=readWave(LW[i])
writeWave(normalize(WavRS,unit="16"),filename=gsub(DirW,DirO,LW[i])
          ,extensible=F) #ecrit le nouveau fichier wave
  
}

