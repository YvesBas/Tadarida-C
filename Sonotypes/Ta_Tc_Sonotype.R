library(beepr)
require(plyr)

ClassifC1="ClassifC1_Sonotype.R"
AggContacts="AggContacts_Sonotype.R"
AggNbSp="AggNbSp_Sonotype.R"

#get arguments from the command line
args <- commandArgs(trailingOnly = TRUE)

if(length(args)<3) #for local tests
  {
  ClassifC1="C:/Users/croemer01/Documents/R/Tadarida_GitHub/Tadarida-C/tadaridaC_src/ClassifC1_Sonotype.R"
  AggContacts="C:/Users/croemer01/Documents/R/Tadarida_GitHub/Tadarida-C/tadaridaC_src/AggContacts_Sonotype.R"
  AggNbSp="C:/Users/croemer01/Documents/R/Tadarida_GitHub/Tadarida-C/tadaridaC_src/AggNbSp_Sonotype.R"
    args="F:/Sons utilisés pour papier Sonotypes/PourScan_Valley_of_fire/txt" # Directory containing .ta files of sounds to classify
#args[1]="D:/PI_20/DataPR_Net1&2/txt"
args[2]="C:/Users/croemer01/Documents/ClassifEsp__tabase3HF_sansfiltre_2020-12-11.learner"
args[3]="N"

#options (HPF = filtre passe-haut / Reduc = reduction des features par DFA)
args[4]=8 #High-pass Filter
args[5]=F #Reduc - obsolete
args[6]=F #TC
args[7]=1000 #block size
args[10]="C:/Users/croemer01/Documents/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Classificateur/SpeciesListComplete.csv" #species list
args[11]="CNS_tabase3HF_France_IdConc.learner" #name of the species number" classifier
args[12]=F #if species number should be filtered or not
args[13]=NA # "Referentiel_seuils_ProbEspHF_.csv"
args[14]=80000 #an additionnal, larger block size to handle memory leaks problem in randomForest
args[15]="ClassifEsp_tabase3HF_France_Cir_2019-11-26_wiSR.learner"
args[16]="CNS_RSDB_HF_tabase3HF_sansfiltre_IdTot_wiSR_IdConc.learner"
args[17]="Referentiel_seuils_RSDB_HF_tabase3HF_sansfiltre_IdTot_wiSR_IdConc__G.csv"
args[18]=1 #block number 
args[19]="FreqMP" # The variable used to build modes, i.e. to build groups of calls
args[20]=0.05 # the threshold of PED used to separate secondary species
#args[21]="20191108_212316_000.ta" # If provided, the script will only run for this file
args[22]=24 # Version number of the classifier
}

print(getwd())
print(args)

tadir=args[1] # Directory containing .ta files of sounds to classify
talistot=list.files(tadir,pattern=".ta$",full.names=T) # Lists .ta files

if (!is.na(args[21])){
  talistot=subset(talistot, talistot == paste0(args[1], "/", args[21]))
}

START=Sys.time()

# Applies classifier and sorts results by sonotype
if(length(talistot)>as.numeric(args[14])*(as.numeric(args[18])-1))
{
  # Subsets blocks of .ta files to process
  talistot=talistot[(as.numeric(args[14])*(as.numeric(args[18])-1)+1)
                    :(min(length(talistot)
                          ,as.numeric(args[14])*(as.numeric(args[18]))))]
  
  FITA=file.info(talistot)
  talistot=subset(talistot,FITA$size>1000)
  
  block=as.numeric(args[7])
  
  # Runs the successive scripts on the block
  for (r in 1:ceiling(length(talistot)/block))
  {
    args[8]=block*(r-1)+1 #start number
    args[9]=block*r #end number
    source(ClassifC1) # Loads classifiers and runs it to give predictions
    print(paste(r,ceiling(length(talistot)/block),Sys.time()))
    if(!skip){
      source(AggContacts) # Aggregate predictions
      source(AggNbSp) # Aggregates Number of species (?)
    }
    
    print(gc())
    
  }
}

# Rbind all IdTot

NomFichier="IdTot_TOTAL"

ls1 <- list.files(args[1], recursive=FALSE,pattern="*IdTot.csv$")
ls2 = paste(args[1], "/", ls1, sep="")
ld <- lapply(ls2, function(x) read.csv(x, header = TRUE, sep= ",", dec = ".", fill = TRUE))

TableID <- do.call("rbind.fill", ld)
TableID$Commentaire=""
TableID$IdMan=""

write.table(TableID,paste(args[1], "/", NomFichier, "_V", args[22], 
                          ".csv", sep=""),
            sep=";",row.names=F)

# Summary file

NomFichiersummary="IdTot_TOTAL_summary"
TableIDsummary=as.data.frame(table(TableID$SpMaxF2))
write.table(TableIDsummary,paste(args[1], "/", NomFichiersummary, "_V", args[22], 
                          ".csv", sep=""),
            sep=";",row.names=F)


beep(2)
END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF
