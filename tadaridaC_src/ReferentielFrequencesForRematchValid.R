library(data.table)

tabase=fread("RSDB_HF_tabase3HF_sansfiltre.csv")
SpeciesList=fread("SpeciesList.csv")


SpFmax=aggregate(tabase$Fmax,by=list(tabase$Espece),median)
SpFmin=aggregate(tabase$Fmin,by=list(tabase$Espece),median)
SpFmed=(SpFmax$x+SpFmin$x)/2

RefFreqSp1=data.frame(cbind(SpFmax,SpFmin$x,SpFmed))

CorrEsp=match(tabase$Espece,SpeciesList$Esp)
tabase$Nesp1=SpeciesList$Nesp[CorrEsp]

tN1=subset(tabase,!(tabase$Nesp1 %in% RefFreqSp1$Group.1))
table(tN1$Nesp1)

SpFmax=aggregate(tN1$Fmax,by=list(tN1$Nesp1),median)
SpFmin=aggregate(tN1$Fmin,by=list(tN1$Nesp1),median)
SpFmed=(SpFmax$x+SpFmin$x)/2

RefFreqSp2=data.frame(cbind(SpFmax,SpFmin$x,SpFmed))

RefFreqSp12=rbind(RefFreqSp1,RefFreqSp2)

tabase$Nesp2=SpeciesList$Nesp2[CorrEsp]

tN2=subset(tabase,!(tabase$Nesp2 %in% RefFreqSp12$Group.1))
table(tN2$Nesp2)

SpFmax=aggregate(tN2$Fmax,by=list(tN2$Nesp2),median)
SpFmin=aggregate(tN2$Fmin,by=list(tN2$Nesp2),median)
SpFmed=(SpFmax$x+SpFmin$x)/2

RefFreqSp3=data.frame(cbind(SpFmax,SpFmin$x,SpFmed))

RefFreqSp123=rbind(RefFreqSp12,RefFreqSp3)

fwrite(RefFreqSp123,"RefFreqSp123.csv",sep=";")

#test=subset(RefFreqSp,RefFreqSp$Group.1=="piaf")
