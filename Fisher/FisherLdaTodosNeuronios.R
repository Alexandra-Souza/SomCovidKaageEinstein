library(MVar.pt)

url <- "C:/Covid19/Fisher/NeuroniosTodos.CSV"
arquivoNeuTodos=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)


str(arquivoNeuTodos,list.len = 300)

arquivoNeuTodosNumerico=arquivoNeuTodos

for (coluna in 3:7){
  arquivoNeuTodosNumerico[,coluna] <- as.double(arquivoNeuTodos[,coluna]) 
}

dataNeuTodos  = arquivoNeuTodosNumerico[,3:6] # dados a serem classificados
classTodos = arquivoNeuTodosNumerico[,7]   # classe dos dados
prior = c(1,1)/2 # probabilidade a priori das classes

ResTodos <- DA(dataNeuTodos, classTodos, type = "lda", validation = "learning", 
            method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); ResTodos$confusion
print("Proporcao global de acerto:"); 1 - ResTodos$error.rate
print("Probabilidade das classes:"); ResTodos$prior
print("Metodo de classificacao:"); ResTodos$method
print("Tipo analise discriminante:"); ResTodos$type
print("Nomes das classes:"); ResTodos$class.names
print("Numero de classes:"); ResTodos$num.class
print("Tipo de validacao:"); ResTodos$validation
print("Numero de observacoes corretas:"); ResTodos$num.correct
print("Matriz com os resultados da classificacao:"); ResTodos$results



### validacao cruzada ###
amostraNeuTodos   = sample(2, nrow(dataNeuTodos), replace = TRUE, prob = c(0.7,0.3))
datatrainNeuTodos = dataNeuTodos[amostraNeuTodos == 1,] # dados para treino
datatestNeuTodos  = dataNeuTodos[amostraNeuTodos == 2,] # dados para teste

dim(datatrainNeuTodos) # dimensao dados treino
dim(datatestNeuTodos)  # dimensao dados teste

testingNeuTodos  = as.integer(rownames(datatestNeuTodos)) # indice dos dados teste

ResQdaNeuTodos <- DA(dataNeuTodos, classTodos, type = "qda", validation = "testing", 
          method = "moment", prior = NA, testing = testingNeuTodos)

print("Tabela de confusao:"); ResQdaNeuTodos$confusion
print("Proporcao global de acerto:"); 1 - ResQdaNeuTodos$error.rate
print("Numero de observacoes corretas:"); ResQdaNeuTodos$num.correct
print("Matriz com os resultados da classificacao:"); ResQdaNeuTodos$results


#####PCA

PC <- PCA(dataNeuTodos, 2) # executa o PCA
print("Matriz de Covariancia/Correlacao:"); round(PC$mtxC,2)
print("Componentes Principais:"); round(PC$mtxAutvec,2)

Tit = c("Scree-plot","Grafico das Observacoes","Circulo de Correlacoes")
Plot.PCA(PC, titles = Tit, xlabel = NA, ylabel = NA,color = TRUE, linlab = NA, casc = TRUE)
