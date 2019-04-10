CC="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/ContextualClassif.R"
args=c("","SpeciesList.csv","ClassifEspC3_2019-03-25.learner"
       ,"C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/f_CombineProbas.r"
       ,"C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/f_Rescale_Probas.r"
       ,"Referentiel_seuils_ProbEspC3_2019-03-25_G7__D_G.csv")
DirTC="./Tadarida/TCsel2/"

source2 <- function(file, skip) {
  file.lines <- scan(file, what=character(), skip=skip, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed))
}


SubDirTC=list.dirs(DirTC,recursive=F)

for (i in 1:length(SubDirTC))
{
 args[1]=SubDirTC[i]
   source2(file=CC,skip=2)
}
