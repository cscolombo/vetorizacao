library(dplyr)
library(stringr)

indexar <- function(x, lista, saida ) {
  if(length(lista) > 1 ){
    fac <- factor(rbind(x[,lista[1]], x[,lista[2]]))
    lev <- levels(fac) 
  }else{
    fac <- factor(x[,lista[1]])
    lev <- levels(fac)
  }
  for (col in lista) {
    x[,col] <- factor(x[,col], levels = lev)
    x[,col] <- as.integer(x[,col] )
  }
  file.name <- paste(saida,"index",sep = "_")
  write.table(lev, paste(file.name, "txt", sep = "."), col.names = F, row.names = F, quote = F)
  return(x)
}

arguments <- commandArgs()
args.treino = 1 
args.inputfile = "TreinoPVetorizarSenhora.csv"
args.outputfile = "Saida"

datasetO <- read.csv(args.inputfile, header = F, sep=" ", stringsAsFactors = F, quote = "")
lcol = 3:18
l_remover = c("ini=", "cap=", "simb=", "prevW=", "prevT=", "prevCap=", "nextW=", "nextT=", "nextCap=", "prev2W=", "prev2T=", "prev2Cap=", "next2W=", "next2T=", "next2Cap=", "palpite=")
for (index in 1:length(l_remover)) {
  datasetO[,lcol[index]] <- str_remove_all(datasetO[,lcol[index]], l_remover[index])
}

dataset <- indexar(datasetO, c(1,6,9,12,15), "tokens")
dataset <- indexar(dataset, c(4,8,11,14,17), "cap")
dataset <- indexar(dataset, c(2,7,10,13,16), "Tipo")
if(args.treino == 1){
  dataset <- indexar(dataset, c(18,19),"tags")
}else{
  dataset <- indexar(dataset, c(18),"tags")
}
dataset <- indexar(dataset, c(3),"ini")
dataset <- indexar(dataset, c(5),"simb")

write.table(dataset, paste(args.outputfile, "txt", sep = "."), col.names = F, row.names = F, quote = F)
