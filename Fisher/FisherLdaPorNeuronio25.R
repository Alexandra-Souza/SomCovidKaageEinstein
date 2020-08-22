library(MVar.pt)


url <- "C:/Covid19/Fisher/Neuronios25.CSV"
arquivoNeu25=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)




str(arquivoNeu25,list.len = 300)

arquivoNeu25Numerico=arquivoNeu25

for (coluna in 3:7){
  arquivoNeu25Numerico[,coluna] <- as.double(arquivoNeu25[,coluna]) 
}


dataNeu25  = arquivoNeu25Numerico[,3:6] # dados a serem classificados
str(dataNeu25,list.len = 300)

class25 = arquivoNeu25Numerico[,7]   # classe dos dados
prior = c(1,1)/2 # probabilidade a priori das classes

Res25 <- DA(dataNeu25, class25, type = "lda", validation = "learning", 
            method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); Res25$confusion
print("Proporcao global de acerto:"); 1 - Res25$error.rate
print("Probabilidade das classes:"); Res25$prior
print("Metodo de classificacao:"); Res25$method
print("Tipo analise discriminante:"); Res25$type
print("Nomes das classes:"); Res25$class.names
print("Numero de classes:"); Res25$num.class
print("Tipo de validacao:"); Res25$validation
print("Numero de observacoes corretas:"); Res25$num.correct
print("Matriz com os resultados da classificacao:"); Res25$results
