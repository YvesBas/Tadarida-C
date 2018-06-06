library(data.table)

args="IdExha_IdConc1.csv"
args[2]="IdExha_IdConc10.csv"
args[3]="C:/wamp64/www/p_export.csv"

microdroitRP<-function(x)
{
  substr(x,nchar(x)-10,nchar(x)-10)=="1"
}



IdE1=fread(args[1])
IdE10=fread(args[2])
Particip=fread(args[3])

IdE1P=merge(IdE1,Particip,by="participation")

IdE1P$MD=microdroitRP(IdE1P$Group.1)

IdE1D=subset(IdE1P,((IdE1P$MD&(IdE1P$canal_enregistrement_direct=="DROITE"))
                    |(!IdE1P$MD&(IdE1P$canal_expansion_temps=="GAUCHE"))))

IdE1Dsel=subset(IdE1D,select=names(IdE10))
                
IdE10E=subset(IdE10,!(IdE10$Group.1 %in% IdE1D$Group.1))

IdEP=rbind(IdE10E,IdE1Dsel)

fwrite(IdEP,"IdExhaustifsTriees.csv")
