#args="tabase3HF_France_IdConc.csv"
args="ProbEspHF.csv"
library(data.table)
ColD="Filename" #indicate where names of data identifier; "Filename" if from C1 Classifier, "Group.1" from C3
ColSp="Espece" #indicate of the "species" column name; "Espece" if from C1 classifier, "SpMaxF2" from C3
SpeciesList=fread("SpeciesList.csv")
Sel="" #mettre "" si pas de sélection sinon "Car" ou "Cir" selon le type de protocole à filtrer


#A EDITER en fonction du classificateur et du jeu de données que l'on veut évaluer
#récupération des votes / tests qui permettent les régressions
Votes=as.data.frame(fread(args))
testD=match(ColD,colnames(Votes))
#sélection des données selon le protocole
if(Sel!=""){
  Votes=subset(Votes,substr(Votes[,testD],1,3)==Sel)
}

#########################################################################
###################### THRESHOLD ANALYSES ###############################
#########################################################################
library(boot)


#groupement des espèces si besoin
test=match(ColSp,colnames(Votes))
Espece0=Votes[,test]
testE=match(Espece0,SpeciesList$Esp)
Espece0[is.na(testE)]="Pleaur"
testE=match(Espece0,SpeciesList$Esp)
print(table(subset(Espece0,is.na(testE))))
Votes$Espece=SpeciesList$Nesp[testE]

#sélection de la liste d'espèces avec suffisamment de données positives
tabSp=table(Votes$Espece)
ListSp=subset(row.names(tabSp),tabSp>10)
test=match(ListSp,colnames(Votes)) # récupère la colonne qui contient l'indice de confiance pour l'espèce i
ListSp=subset(ListSp,is.na(test)==F) #retire les espèces manquantes (normalement ça n'arrive pas mais possible incohérence SpeciesList)

#initialise la page de graphes
#par(oma=c(2,2,0,0),mar=c(3,3,2,2),mfrow=c(4,3))
#title("", outer=TRUE)
#mtext(text="Confidence index of the automatic identification",side=1,line=0,outer=TRUE)
#mtext(text="Success probability",side=2,line=0,outer=TRUE)

par(mfrow=c(1,1))

Int=vector() #pour stocker l'intercept de la logistique
Pente=vector() #pour stocker la pente de la logistique
Seuil50=vector() #pour stocker le seuil de tolérance maximal de risque d'erreur de 50 %
Seuil90=vector() #pour stocker le seuil de tolérance maximal de risque d'erreur de 10 %
Seuil95=vector() #pour stocker le seuil de tolérance maximal de risque d'erreur de 5 %
Seuil99=vector() #pour stocker le seuil de tolérance maximal de risque d'erreur de 1 %


FN50=vector() # pour stocker le taux de faux négatifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 50
FN90=vector() # pour stocker le taux de faux négatifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 90
FN95=vector() # pour stocker le taux de faux négatifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 95
FN99=vector() # pour stocker le taux de faux négatifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 99

FP50=vector() # pour stocker le taux de faux positifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 50
FP90=vector() # pour stocker le taux de faux positifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 90
FP95=vector() # pour stocker le taux de faux positifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 95
FP99=vector() # pour stocker le taux de faux positifs (parmi les validations et non parmi les données, attention !!!) induit par le seuil 99

Nvalid=vector() # pour stocker le nombre de validations par espèces
for (i in 1:length(ListSp)) #12 min sur la couche n°1
{
  SpCol=as.numeric(match(ListSp[i],colnames(Votes))) # récupère la colonne qui contient l'indice de confiance pour l'espèce i
  VoteEsp=as.data.frame(Votes)[,SpCol]
  if(ColD=="Filename" )
  {
    VoteAgg=aggregate(VoteEsp,by=list(Votes[,testD]),FUN=max)
    test=match(paste(Votes[,testD],VoteEsp),paste(VoteAgg$Group.1,VoteAgg$x))
    Votesp0=subset(Votes,is.na(test)==F)
    Votesp=unique(as.data.table(Votesp0),by=ColD)
    }else{
    Votesp=Votes
  }
  VoteEsp=as.data.frame(Votesp)[,SpCol]
  
  succes=(Votesp$Espece==ListSp[i]) #succes si l'espèce i n'obtient pas le score max
  print(paste(i, ListSp[i],sum(as.numeric(succes)),Sys.time()))
  
  
  mvotes<-glm(succes ~ VoteEsp, family=binomial(link = logit)) # modélisation de la logistique
  mvotes_probit<-glm(succes ~ VoteEsp, family=binomial(link = probit)) # modélisation de la logistique
  
  
  summary(mvotes)
  AIC(mvotes,mvotes_probit)
  
  #jpeg(paste0("Logistique_",substr(args,1,nchar(args)-4),"_",Sel,"_",ListSp[i],".jpg")) #crée un fichier jpeg
  #plot les erreurs / succès sur le graphe
  plotvotes<-plot(succes ~ VoteEsp, xlim=c(0, 1), ylim=c(0,1),main=ListSp[i],font.main= 4,xlab="Confidence index of the automatic identification", ylab="Success probability")
  absc<-seq(0,1,0.001) #crée une échelle de prédiction tous les 1e-3
  ND=data.frame(VoteEsp=absc)
  y=predict(mvotes,newdata=ND,type="response")
  
  y2=predict(mvotes_probit,newdata=ND,type="response")
  
  lines(absc, y,col=2) # ajoute la logistique (en rouge)
  lines(absc, y2,col=3) # ajoute la logistique (en rouge)
  
  abline(h=0.5, col="grey",lwd=2) # ajoute la barre déterminant le seuil 50
  abline(h=0.9, col="grey50",lwd=2) # ajoute la barre déterminant le seuil 90
  dev.off() # envoie le graphe dans le fichier jpeg
  Int=c(Int,mvotes$coefficients[1]) 
  Pente=c(Pente,mvotes$coefficients[2])
  Seuil50=c(Seuil50,min(subset(absc,y>0.5)))
  Seuil90=c(Seuil90,min(subset(absc,y>0.9)))
  Seuil95=c(Seuil95,min(subset(absc,y>0.95)))
  Seuil99=c(Seuil99,min(subset(absc,y>0.99)))
  
  FN50=c(FN50,sum(subset(succes,VoteEsp<min(subset(absc,y>0.5))))/sum(succes))
  FN90=c(FN90,sum(subset(succes,VoteEsp<min(subset(absc,y>0.9))))/sum(succes))
  FN95=c(FN95,sum(subset(succes,VoteEsp<min(subset(absc,y>0.95))))/sum(succes))
  FN99=c(FN99,sum(subset(succes,VoteEsp<min(subset(absc,y>0.99))))/sum(succes))
  
  FP50=c(FP50,sum(subset((1-succes),VoteEsp>=min(subset(absc,y>0.5))))/nrow(subset(Votesp,VoteEsp>=min(subset(absc,y>0.5)))))
  FP90=c(FP90,sum(subset((1-succes),VoteEsp>=min(subset(absc,y>0.9))))/nrow(subset(Votesp,VoteEsp>=min(subset(absc,y>0.9)))))
  FP95=c(FP95,sum(subset((1-succes),VoteEsp>=min(subset(absc,y>0.95))))/nrow(subset(Votesp,VoteEsp>=min(subset(absc,y>0.9)))))
  FP99=c(FP99,sum(subset((1-succes),VoteEsp>=min(subset(absc,y>0.99))))/nrow(subset(Votesp,VoteEsp>=min(subset(absc,y>0.9)))))
  
  Nvalid=c(Nvalid,sum(succes))
}

Referentiel_seuils=as.data.frame(cbind(Espece=ListSp,Int,Pente
                                       ,Seuil50,Seuil90,Seuil95,Seuil99,FN50,FN90,FN95,FN99
                                       ,FP50,FP90,FP95,FP99,Nvalid))

fwrite(Referentiel_seuils,paste0("Referentiel_seuils_",substr(args,1,nchar(args)-4),"_",Sel,".csv"),row.names=F)
