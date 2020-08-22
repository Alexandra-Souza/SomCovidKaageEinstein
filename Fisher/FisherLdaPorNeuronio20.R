library(MVar.pt)


url <- "C:/Covid19/Fisher/Neuronios20.CSV"
arquivoNeu20=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)




str(arquivoNeu20,list.len = 300)

arquivoNeu20Numerico=arquivoNeu20

for (coluna in 3:7){
  arquivoNeu20Numerico[,coluna] <- as.double(arquivoNeu20[,coluna]) 
}


dataNeu20  = arquivoNeu20Numerico[,3:6] # dados a serem classificados
str(dataNeu20,list.len = 300)

class20 = arquivoNeu20Numerico[,7]   # classe dos dados
prior = c(1,1)/2 # probabilidade a priori das classes

Res20 <- DA(dataNeu20, class20, type = "lda", validation = "learning", 
            method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); Res20$confusion
print("Proporcao global de acerto:"); 1 - Res20$error.rate
print("Probabilidade das classes:"); Res20$prior
print("Metodo de classificacao:"); Res20$method
print("Tipo analise discriminante:"); Res20$type
print("Nomes das classes:"); Res20$class.names
print("Numero de classes:"); Res20$num.class
print("Tipo de validacao:"); Res20$validation
print("Numero de observacoes corretas:"); Res20$num.correct
print("Matriz com os resultados da classificacao:"); Res20$results
