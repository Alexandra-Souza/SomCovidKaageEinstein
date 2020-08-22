library(MVar.pt)


url <- "C:/Covid19/Fisher/Neuronios5.CSV"
arquivoNeu5=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)




str(arquivoNeu5,list.len = 300)

arquivoNeu5Numerico=arquivoNeu5

for (coluna in 3:7){
  arquivoNeu5Numerico[,coluna] <- as.double(arquivoNeu5[,coluna]) 
}


dataNeu5  = arquivoNeu5Numerico[,3:6] # dados a serem classificados
str(dataNeu5,list.len = 300)

class5 = arquivoNeu5Numerico[,7]   # classe dos dados
prior = c(1,1)/2 # probabilidade a priori das classes

Res5 <- DA(dataNeu5, class5, type = "lda", validation = "learning", 
            method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); Res5$confusion
print("Proporcao global de acerto:"); 1 - Res5$error.rate
print("Probabilidade das classes:"); Res5$prior
print("Metodo de classificacao:"); Res5$method
print("Tipo analise discriminante:"); Res5$type
print("Nomes das classes:"); Res5$class.names
print("Numero de classes:"); Res5$num.class
print("Tipo de validacao:"); Res5$validation
print("Numero de observacoes corretas:"); Res5$num.correct
print("Matriz com os resultados da classificacao:"); Res5$results
