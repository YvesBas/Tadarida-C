library(data.table)
SpeciesList=fread("SpeciesList.csv")
SpRSDBLF=fread("RSDB_LF_NbSiteEsp.csv")
SpRSDBHF=fread("RSDB_HF_NbSiteEsp.csv")
SpRSDB=rbind(SpRSDBLF,SpRSDBHF)

SpRSDB=SpRSDB[order(SpRSDB$x,decreasing=T)]
SpRSDBu=unique(SpRSDB,by="Group.1")

CommonSpecies=SpRSDBu[1:floor(nrow(SpRSDBu)/2),]

test=subset(SpRSDB,SpRSDB$Group.1=="Nemsyl")

CommonSpeciesL=merge(SpeciesList,CommonSpecies,by.x="Esp",by.y="Group.1")

fwrite(CommonSpeciesL,"CommonSpeciesL.csv")
