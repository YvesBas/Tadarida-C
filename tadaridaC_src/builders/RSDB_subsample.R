library(data.table)

tabase=fread("RSDB_LF_tabase3HF_sansfiltre.csv")
Filtre=fread("C:/Users/Yves Bas/Downloads/lignes_base_108.txt")
#Filtre=fread("C:/Users/Yves Bas/Downloads/lignes_jeutest_108.txt")
ListSp=c("Stralu","Locnae","Capeur","Turmer","Turphi","Erirub","Lusmeg")


if(names(Filtre)[1]=="V1")
{
  names(Filtre)=c(names(Filtre)[2:ncol(Filtre)],"V")
}
test2=paste(Filtre$fichier,Filtre$numcri)
test1=paste(tabase$Filename,tabase$CallNum)

summary(test2 %in% test1)
Missed=subset(Filtre,!test2 %in% test1)
head(Missed)
testM=subset(tabase,tabase$Filename=="TF708102_20170504_215206.wav")

Selected=subset(tabase,test1 %in% test2)
table(Selected$Nesp)
#test=subset(Selected,Selected$Espece=="mamm")
Selected=subset(Selected,Selected$Nesp %in% ListSp)
table(Selected$Nesp)

fwrite(Selected,paste0("RSDBsel_",Sys.Date(),".csv"),sep=";")
