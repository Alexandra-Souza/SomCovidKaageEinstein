library(MVar.pt)


url <- "C:/Covid19/Fisher/Neuronios12.CSV"
arquivoNeu12=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)




str(arquivoNeu12,list.len = 300)

arquivoNeu12Numerico=arquivoNeu12

for (coluna in 3:7){
  arquivoNeu12Numerico[,coluna] <- as.double(arquivoNeu12[,coluna]) 
}


dataNeu12  = arquivoNeu12Numerico[,3:6] # dados a serem classificados
str(dataNeu12,list.len = 300)

class12 = arquivoNeu12Numerico[,7]   # classe dos dados
prior = c(1,1)/2 # probabilidade a priori das classes

Res12 <- DA(dataNeu12, class12, type = "lda", validation = "learning", 
            method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); Res12$confusion
print("Proporcao global de acerto:"); 1 - Res12$error.rate
print("Probabilidade das classes:"); Res12$prior
print("Metodo de classificacao:"); Res12$method
print("Tipo analise discriminante:"); Res12$type
print("Nomes das classes:"); Res12$class.names
print("Numero de classes:"); Res12$num.class
print("Tipo de validacao:"); Res12$validation
print("Numero de observacoes corretas:"); Res12$num.correct
print("Matriz com os resultados da classificacao:"); Res12$results
