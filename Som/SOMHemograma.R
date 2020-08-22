
source('coolBlueHotRed.R')


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


qualiHemotrain<-as.matrix(arquivoHemoNumerico[,c(8:21)],na.rm = TRUE)
str(qualiHemotrain,list.len = 300)


#qualiHemotrain.normal = normalizeData(qualiHemotrain, type = "0_1")

# 603 linhas-> raiz=24 -> grid 5x5
#calculo qtde epocas -> 1000/log5 = 1432
#                       500x25(quinhentas vezes o numero de neuronio na grade)= 12500
#                       12.500+1432= 13.932 -> 14.000


somqualiHemotrain<- supersom(qualiHemotrain,grid=somgrid(5,5,"hexagonal"),rlen=14000,alpha=c(0.05,0.01)
                             ,keep.data = TRUE, dist.fcts = "euclidean", normalizeDataLayers = "FALSE")

par(mfrow = c(2,2))
for (resultSARS_Cov_2 in 0:1){
  indices<- as.numeric (arquivoHemoNumerico$SARS.Cov.2.exam.result.BIN)
  for (x in 1:598){
    if (indices[x] == resultSARS_Cov_2){
      indices[x]=resultSARS_Cov_2    
    }else{
      indices[x]=" "
    }
  }
  plot(somqualiHemotrain, type="mapping",
       labels =  as.integer(indices), col = 1,
       cex=font_size,
       main = "SARS-Cov-2: 0 Negativo, 1 Positivo")
}
summary(somqualiHemotrain)

pdf("Qualidade.pdf", width = 6, height = 6) 
par(mfrow = c(1,1))

#mostra a distância média dos exemplares mapeados para o nó em relaçao ao seu vetor de pesos.
plot(somqualiHemotrain, type="quality",main = "Distancia Média dos exemplares no Neuronio (coesão)",
     palette.name =rainbow)

#mostra a distancia entre os neuronios vizinhos
plot(somqualiHemotrain, type="dist.neighbours", main = "Distancia Média do Neuronio Vizinho (dispersão)",
     palette.name =rainbow) 

#mostra o numero de exemplares mapeados em cada nó
counts <- plot(somqualiHemotrain, type="counts")

dev.off()

#distancia média do fechada do vetor de pesos durante o treinamento
plot(somqualiHemotrain, type="changes")




pdf("codes.pdf", width = 20, height = 14) 
par(mfrow = c(1,1))
#mostra o vetor de pesos de cada nó
plot(somqualiHemotrain, type="codes",codeRendering ="segments",  palette.name=rainbow)
somqualiHemotrain$codes
dev.off()

#cada Paciente positivo e negativo em SARS-Cov-2 foi classificado
pdf("ClustercomSARS-Cov-2.pdf", width = 6, height = 6) 
par(mfrow = c(2,2))
for (resultSARS_Cov_2 in 0:1){
  indices<- as.numeric (arquivoHemoNumerico$SARS.Cov.2.exam.result.BIN)
  for (x in 1:598){
    if (indices[x] == resultSARS_Cov_2){
      indices[x]=resultSARS_Cov_2    
    }else{
      indices[x]=" "
    }
  }
  plot(somqualiHemotrain, type="mapping",
       labels =  as.integer(indices), col = 1,
       cex=font_size,
    #   bgcol = pretty_palette[som_cluster],
       main = "SARS-Cov-2: 0 Negativo, 1 Positivo")
   #add.cluster.boundaries(somqualiHemotrain, som_cluster)
   }
dev.off()



plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,1],
     main = "Hematocrit",palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,2],
     main = "Hemoglobin",palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,3],
     main = "Platelets", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,4],
     main = "Mean.platelet.volume", palette.name=rainbow)


plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,5],
     main = "Red.blood.Cells", palette.name=rainbow)


plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,6],
     main = "Lymphocytes", palette.name=rainbow)


plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,7],
     main = "Mean.corpuscular.hemoglobin.concentration..MCHC", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,8],
     main = "Leukocytes", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,9],
     main = "Basophils", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,10],
     main = "Mean.corpuscular.hemoglobin..MCH.", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,11],
     main = "Eosinophils", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,12],
     main = "Mean.corpuscular.volume..MCV.", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,13],
     main = "Monocytes", palette.name=rainbow)

plot(somqualiHemotrain, type = "property", property = getCodes(somqualiHemotrain, 1)[,14],
     main = "Red.blood.cell.distribution.width..RDW.", palette.name=rainbow)



