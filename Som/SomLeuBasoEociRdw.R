# SOM treinamento com as variaveis:
#Leukocytes	Basophils	Eosinophils	Red blood cell distribution width (RDW)

########################Cores para os graficos
# Pallet de cores para o mapa de calor
source('coolBlueHotRed.R')


# Definição da cor do pallete para graficos de cluster
pretty_palette <- c('#FFFF33','#99FF99','#7093DB','#FFCCFF','#FF6633','#CC66FF','#CCFFFF','#DEEBF7')
pretty_palette_gray=gray.colors(5, start = 0.4, end = 1, gamma = 2.2, alpha = NULL)
font_size=1

url <- "Hemograma4.CSV"
arquivoHemo=read.table(file=url,header=TRUE,sep=";",colClasses="character",fill = TRUE, stringsAsFactors=FALSE)




str(arquivoHemo,list.len = 300)

arquivoHemoNumerico=arquivoHemo
str(arquivoHemoNumerico,list.len = 300)

for (coluna in 8:21){
  arquivoHemoNumerico[,coluna] <- as.double(arquivoHemo[,coluna]) 
}

str(arquivoHemoNumerico,list.len = 300)


qualiHemotrain<-as.matrix(arquivoHemoNumerico[,c(15,16,18,21)],na.rm = TRUE)
str(qualiHemotrain,list.len = 300)


#qualiHemotrain.normal = normalizeData(qualiHemotrain, type = "0_1")

# 599 linhas-> raiz=24 -> grid 5x5
#calculo qtde epocas -> 1000/log5 = 1432
#                       500x25(quinhentas vezes o numero de neuronio na grade)= 12500
#                       12.500+1432= 13.932 -> 14.000

pdf("QualidadeBasoBatch.pdf", width = 6, height = 6) 

for (index in 1:100){

#somqualiHemoBasotrain<- supersom(qualiHemotrain.normal,grid=somgrid(5,5,"hexagonal"),rlen=14000,alpha=c(0.15,0.04)
#                             ,keep.data = TRUE, dist.fcts = "euclidean", normalizeDataLayers = "FALSE")

somqualiHemoBasotrain<- supersom(qualiHemotrain,grid=somgrid(5,5,"hexagonal"),rlen=14000,alpha=c(0.08,0.03)
                                 ,keep.data = TRUE, dist.fcts = "euclidean", normalizeDataLayers = "FALSE")


counts <- plot(somqualiHemoBasotrain, type="counts")


nomeImage=c("Train",index)

save.image(file = paste0(index, ".RData"))

cat("\n Index", index)

}
dev.off()

summary(somqualiHemoBasotrain)

######################## Analises
pdf("Qualidade.pdf", width = 6, height = 6) 
par(mfrow = c(1,1))

#mostra a distância média dos exemplares mapeados para o nó em relaçao ao seu vetor de pesos.
plot(somqualiHemoBasotrain, type="quality",main = "Distancia Média dos exemplares no Neuronio (coesão)",
     palette.name =rainbow)

#mostra a distancia entre os neuronios vizinhos
plot(somqualiHemoBasotrain, type="dist.neighbours", main = "Distancia Média do Neuronio Vizinho (dispersão)",
     palette.name =rainbow) 

#mostra o numero de exemplares mapeados em cada nó
counts <- plot(somqualiHemoBasotrain, type="counts")

dev.off()

#distancia média do fechada do vetor de pesos durante o treinamento
plot(somqualiHemoBasotrain, type="changes")




pdf("codesHemoBaso.pdf", width = 20, height = 14) 
par(mfrow = c(1,1))
#mostra o vetor de pesos de cada nó
plot(somqualiHemoBasotrain, type="codes",codeRendering ="segments",  palette.name=rainbow)
somqualiHemoBasotrain$codes
dev.off()

pdf("MapaCalorHemoBaso.pdf", width = 20, height = 14) 



plot(somqualiHemoBasotrain, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,1],
     main = "Leucocytes", palette.name=rainbow)

plot(somqualiHemoBasotrain, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,2],
     main = "Basophils", palette.name=rainbow)

plot(somqualiHemoBasotrain, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,3],
     main = "Eosinophils", palette.name=rainbow)

plot(somqualiHemoBasotrain, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,4],
     main = "Red.blood.cell.distribution.width..RDW.", palette.name=rainbow)


dev.off()

vetClassif = table(somqualiHemoBasotrain$unit.classif)
print(vetClassif)
ArquiVetClassif<- data.frame(somqualiHemoBasotrain$unit.classif)
write.table(ArquiVetClassif,file='Neuronios.csv',sep=',',na="",quote=TRUE, row.names=FALSE)



#cada Paciente positivo e negativo em SARS-Cov-2 foi classificado
pdf("ClustercomSARS-Cov-2Baso.pdf", width = 6, height = 6) 
par(mfrow = c(2,2))
resultSARS_Cov_2 = 0
for (resultSARS_Cov_2 in 0:1){
  indices<- as.numeric (arquivoHemoNumerico$SARS.Cov.2.exam.result.BIN)
  for (x in 1:598){
    if (indices[x] == resultSARS_Cov_2){
      indices[x]  
    }else{
      indices[x]=" "
    }
  }
  plot(somqualiHemoBasotrain, type="mapping", 
       labels =  as.integer(indices), col = 1,
       cex=font_size,
       main = "SARS-Cov-2: 0 Negativo, 1 Positivo")
}
dev.off()

pdf("Clustercom Patient addmitedBaso.pdf", width = 6, height = 6) 

#Paciente n�o hospitalizado?
par(mfrow = c(2,2))
resultRegularWard = 0
#for (resultSARS_Cov_2 in 0:1){
vetresultSARS_Cov_2<-as.numeric (arquivoHemoNumerico$SARS.Cov.2.exam.result.BIN)
indices<- as.numeric (arquivoHemoNumerico$Patient.addmited.to.regular.ward..1.yes..0.no.)
for (x in 1:598){
  if (vetresultSARS_Cov_2[x] == 1){
    if (indices[x] ==0){
      indices[x]   
    }else{
      indices[x]=" "
    }
  }else {indices[x]=" "}
  
}    
plot(somqualiHemoBasotrain, type="mapping",
     labels =  as.integer(indices), col = 1,
     cex=font_size,
     main = "Patient n�o hospitalizado?")

#cada Paciente Patient addmited to regular ward foi classificado
resultRegularWard = 0
vetresultSARS_Cov_2<-as.numeric (arquivoHemoNumerico$SARS.Cov.2.exam.result.BIN)
indices<- as.numeric (arquivoHemoNumerico$Patient.addmited.to.regular.ward..1.yes..0.no.)
for (x in 1:598){
  if (vetresultSARS_Cov_2[x] == 1){
    if (indices[x] ==1){
      indices[x]   
    }else{
      indices[x]=" "
    }
  }else {indices[x]=" "}
  
}    
plot(somqualiHemoBasotrain, type="mapping",
     labels =  as.integer(indices), col = 1,
     cex=font_size,
     main = "Addmited to Regular Ward")


#cada Paciente Patient addmited to semi-intensive unit foi classificado

resultRegularWard = 0
vetresultSARS_Cov_2<-as.numeric (arquivoHemoNumerico$SARS.Cov.2.exam.result.BIN)
indices<- as.numeric (arquivoHemoNumerico$Patient.addmited.to.semi.intensive.unit..1.yes..0.no.)
for (x in 1:598){
  if (vetresultSARS_Cov_2[x] == 1){
    if (indices[x] ==1){
      indices[x]   
    }else{
      indices[x]=" "
    }
  }else {indices[x]=" "}
  
}    
plot(somqualiHemoBasotrain, type="mapping",
     labels =  as.integer(indices), col = 1,
     cex=font_size,
     main = "Addmited to Semi-intensive Unit")

#cada Patient addmited to intensive care unit foi classificado

resultRegularWard = 0
vetresultSARS_Cov_2<-as.numeric (arquivoHemoNumerico$SARS.Cov.2.exam.result.BIN)
indices<- as.numeric (arquivoHemoNumerico$Patient.addmited.to.intensive.care.unit..1.yes..0.no.)
for (x in 1:598){
  if (vetresultSARS_Cov_2[x] == 1){
    if (indices[x] ==1){
      indices[x]   
    }else{
      indices[x]=" "
    }
  }else {indices[x]=" "}
  
}    
plot(somqualiHemoBasotrain, type="mapping",
     labels =  as.integer(indices), col = 1,
     cex=font_size,
     main = "Addmited to Intensive Care Unit")

dev.off()

################Clusteriza??o

#
#clusterização
require(NbClust)


# clusters usando agrupamento particionado no vetor de pesos

gruposK=kmeans(getCodes(somqualiHemoBasotrain),3) # 3 pra frente estabeliza

par(mfrow = c(1,1))

#par(mar = c(0,5,0,2))

plot(somqualiHemoBasotrain, 
     type="codes", 
     codeRendering ="segments",
     palette.name=rainbow,
     bgcol = pretty_palette[gruposK$cluster], 
     main = "k-mean cluster"
)
add.cluster.boundaries(somqualiHemoBasotrain, gruposK$cluster)



# clusters usando agrupamento hierarquico no vetor de pesos
som_cluster <- cutree(hclust(dist(getCodes(somqualiHemoBasotrain),method = "manhattan")),3)# pelo metodo do cotovelo (wss) 3 pra frente estabeliza
plot(somqualiHemoBasotrain, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(somqualiHemoBasotrain, som_cluster)

pdf("ClusterHierarquicocomCodes.pdf", width = 6, height = 6) 
par(mfrow = c(1,1))
plot(somqualiHemoBasotrain, type="codes", codeRendering ="segments",palette.name=rainbow,bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(somqualiHemoBasotrain, som_cluster)
dev.off()

pdf("ClusterHierarquico.pdf", width = 6, height = 6) 
par(mfrow = c(1,1))

#validando solução de clusters
# comparing 2 cluster solutions, calculando o indice de similariedade dos dois clusters
library(fpc)
cluster.stats(dist(getCodes(somqualiHemoBasotrain)), gruposK$cluster, som_cluster, compareonly=TRUE) 
# se corrected.rand 
# se vi < 2logk, sendo k o numero de clusters. 
# como o vi=1.189513, indica que ambos tipos de clsuterização são similares, por isso adotei o hierarquico que apresentou um conjunto de clusters mais coerentes



# show the WCSS metric for kmeans for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
hemoCodes <- getCodes(somqualiHemoBasotrain)
wss <- (nrow(hemoCodes)-1)*sum(apply(hemoCodes,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(hemoCodes,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

