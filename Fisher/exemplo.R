library(MVar.pt)

data(iris) # conjunto de dados

data  = iris[,1:4] # dados a serem classificados
class = iris[,5]   # classe dos dados
prior = c(1,1,1)/3 # probabilidade a priori das classes

Res <- DA(data, class, type = "lda", validation = "learning", 
          method = "mle", prior = prior, testing = NA)

print("Tabela de confusao:"); Res$confusion
print("Proporcao global de acerto:"); 1 - Res$error.rate
print("Probabilidade das classes:"); Res$prior
print("Metodo de classificacao:"); Res$method
print("Tipo analise discriminante:"); Res$type
print("Nomes das classes:"); Res$class.names
print("Numero de classes:"); Res$num.class
print("Tipo de validacao:"); Res$validation
print("Numero de observacoes corretas:"); Res$num.correct
print("Matriz com os resultados da classificacao:"); Res$results


### validacao cruzada ###
amostra   = sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
datatrain = data[amostra == 1,] # dados para treino
datatest  = data[amostra == 2,] # dados para teste

dim(datatrain) # dimensao dados treino
dim(datatest)  # dimensao dados teste

testing  = as.integer(rownames(datatest)) # indice dos dados teste

Res <- DA(data, class, type = "qda", validation = "testing", 
          method = "moment", prior = NA, testing = testing)

print("Tabela de confusao:"); Res$confusion
print("Proporcao global de acerto:"); 1 - Res$error.rate
print("Numero de observacoes corretas:"); Res$num.correct
print("Matriz com os resultados da classificacao:"); Res$results