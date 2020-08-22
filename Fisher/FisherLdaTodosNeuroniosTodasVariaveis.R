library(MVar.pt)

url <- "C:/Covid19/Fisher/NeuroniosTodosVarTodos.CSV"
arquivoNeuTodosVarTodos=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)



str(arquivoNeuTodosVarTodos,list.len = 300)

arquivoNeuTodosVarTodosNumerico=arquivoNeuTodosVarTodos

for (coluna in 3:17){
  arquivoNeuTodosVarTodosNumerico[,coluna] <- as.double(arquivoNeuTodosVarTodos[,coluna]) 
}

str(arquivoNeuTodosVarTodosNumerico,list.len = 300)

dataNeuTodosVarTodos  = arquivoNeuTodosVarTodosNumerico[,3:16] # dados a serem classificados
classTodosVarTodos = arquivoNeuTodosVarTodosNumerico[,17]   # classe dos dados
prior = c(1,1)/2 # probabilidade a priori das classes

ResTodosVarTodos <- DA(dataNeuTodosVarTodos, classTodosVarTodos, type = "lda", validation = "learning", 
               method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); ResTodosVarTodos$confusion
print("Proporcao global de acerto:"); 1 - ResTodosVarTodos$error.rate
print("Probabilidade das classes:"); ResTodosVarTodos$prior
print("Metodo de classificacao:"); ResTodosVarTodos$method
print("Tipo analise discriminante:"); ResTodosVarTodos$type
print("Nomes das classes:"); ResTodosVarTodos$class.names
print("Numero de classes:"); ResTodosVarTodos$num.class
print("Tipo de validacao:"); ResTodosVarTodos$validation
print("Numero de observacoes corretas:"); ResTodosVarTodos$num.correct
print("Matriz com os resultados da classificacao:"); ResTodosVarTodos$results
