library(MVar.pt)


url <- "C:/Covid19/Fisher/Neuronios15.CSV"
arquivoNeu15=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)




str(arquivoNeu15,list.len = 300)

arquivoNeu15Numerico=arquivoNeu15

for (coluna in 3:7){
  arquivoNeu15Numerico[,coluna] <- as.double(arquivoNeu15[,coluna]) 
}


dataNeu15  = arquivoNeu15Numerico[,3:6] # dados a serem classificados
str(dataNeu15,list.len = 300)

class15 = arquivoNeu15Numerico[,7]   # classe dos dados
prior = c(1,1)/2 # probabilidade a priori das classes

Res15 <- DA(dataNeu15, class15, type = "lda", validation = "learning", 
          method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); Res15$confusion
print("Proporcao global de acerto:"); 1 - Res15$error.rate
print("Probabilidade das classes:"); Res15$prior
print("Metodo de classificacao:"); Res15$method
print("Tipo analise discriminante:"); Res15$type
print("Nomes das classes:"); Res15$class.names
print("Numero de classes:"); Res15$num.class
print("Tipo de validacao:"); Res15$validation
print("Numero de observacoes corretas:"); Res15$num.correct
print("Matriz com os resultados da classificacao:"); Res15$results
