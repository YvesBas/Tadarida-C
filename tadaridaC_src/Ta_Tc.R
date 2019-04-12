ClassifC1="ClassifC1.r"
AggContacts="AggContacts.r"
AggNbSp="AggNbSp.r"
#ClassifC1="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/ClassifC1.R"
#AggContacts="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/AggContacts.R"
#AggNbSp="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/AggNbSp.R"




#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
#uncomment the following lines if you pre mfer to do not use R in command line
#args="C:/wamp64/www/wav/txt" #directory containing .ta files
#args[1]="D:/PI_20/DataPR_Net1&2/txt"
#args[2]="ClassifEsp_LF_180320.learner"
#args[2]="ClassifEsp_LF_180129.learner"
#args[2]="ClassifEspFrance180303.learner"
#args[3]="tabase3_LFXC"
#args[3]="N"

#options (HPF = filtre passe-haut / Reduc = réduction des features par DFA)
#args[4]=8 #HPF
#args[5]=F #Reduc - obsolete
#args[6]=F #TC
#args[7]=200 #block size
#args[10]="SpeciesList.csv" #species list
#args[11]="CNS_tabase3HF_France_IdConc.learner" #name of the species number" classifier
#args[12]=T #if species number should be filtered or not
#args[13]="Referentiel_seuils_ProbEspHF_.csv"

print(getwd())
print(args)

tadir=args[1]
talistot=list.files(tadir,pattern=".ta$",full.names=T)
block=as.numeric(args[7])


for (r in 1:ceiling(length(talistot)/block))
  {
args[8]=block*(r-1)+1 #start number
args[9]=block*r #end number
source(ClassifC1)
print(paste(r,ceiling(length(talistot)/block),Sys.time()))
source(AggContacts)
}

print(gc())

source(AggNbSp)
print(gc())

