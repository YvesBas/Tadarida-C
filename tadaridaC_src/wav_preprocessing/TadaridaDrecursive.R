Pattern="wav_"
Dir="C:/Users/yvesb/Documents/www"
#TadDir="C:\Users\yvesb\Documents\Tadarida\Tadarida-D_forWindows\install"

Dir2=list.dirs(Dir,full.names=T,recursive=F)

Dir3=subset(Dir2,grepl(Pattern,Dir2))

for (i in 1:length(Dir3)){
  print(Dir3[i])
#Exp1
Command=paste0("TadaridaD -x 1 -t 16 ",paste0(Dir3,"/ex1"))
system(Command)

print("done x1!")
#Exp10
Command=paste0("TadaridaD -x 10 -t 16 ",paste0(Dir3,"/ex10"))
system(Command)

print("done x10!")
}
