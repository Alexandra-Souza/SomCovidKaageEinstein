# SOM treinamento com as variaveis:
#Leukocytes	Basophils	Eosinophils	Red blood cell distribution width (RDW)

########################Cores para os graficos
# Pallet de cores para o mapa de calor
source('coolBlueHotRed.R')


# DefiniÃ§Ã£o da cor do pallete para graficos de cluster
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

somLeuBasEosRdw<- supersom(qualiHemotrain,grid=somgrid(5,5,"hexagonal"),rlen=14000,alpha=c(0.08,0.03)
                                 ,keep.data = TRUE, dist.fcts = "euclidean", normalizeDataLayers = "FALSE")


counts <- plot(somLeuBasEosRdw, type="counts")


nomeImage=c("Train",index)

save.image(file = paste0(index, ".RData"))

cat("\n Index", index)

}
dev.off()

summary(somLeuBasEosRdw)

######################## Analises
pdf("Qualidade.pdf", width = 6, height = 6) 
par(mfrow = c(1,1))

#mostra a distÃ¢ncia mÃ©dia dos exemplares mapeados para o nÃ³ em relaÃ§ao ao seu vetor de pesos.
plot(somLeuBasEosRdw, type="quality",main = "Distancia MÃ©dia dos exemplares no Neuronio (coesÃ£o)",
     palette.name =rainbow)

#mostra a distancia entre os neuronios vizinhos
plot(somLeuBasEosRdw, type="dist.neighbours", main = "Distancia MÃ©dia do Neuronio Vizinho (dispersÃ£o)",
     palette.name =rainbow) 

#mostra o numero de exemplares mapeados em cada nÃ³
counts <- plot(somLeuBasEosRdw, type="counts")

dev.off()

#distancia mÃ©dia do fechada do vetor de pesos durante o treinamento
plot(somLeuBasEosRdw, type="changes")




pdf("codesHemoBaso.pdf", width = 20, height = 14) 
par(mfrow = c(1,1))
#mostra o vetor de pesos de cada nÃ³
plot(somLeuBasEosRdw, type="codes",codeRendering ="segments",  palette.name=rainbow)
somLeuBasEosRdw$codes
dev.off()

pdf("MapaCalorHemoBaso.pdf", width = 20, height = 14) 



plot(somLeuBasEosRdw, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,1],
     main = "Leucocytes", palette.name=rainbow)

plot(somLeuBasEosRdw, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,2],
     main = "Basophils", palette.name=rainbow)

plot(somLeuBasEosRdw, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,3],
     main = "Eosinophils", palette.name=rainbow)

plot(somLeuBasEosRdw, type = "property", property = getCodes(somqualiHemoBasotrain, 1)[,4],
     main = "Red.blood.cell.distribution.width..RDW.", palette.name=rainbow)


dev.off()

vetClassif = table(somLeuBasEosRdw$unit.classif)
print(vetClassif)
ArquiVetClassif<- data.frame(somLeuBasEosRdw$unit.classif)
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
  plot(somLeuBasEosRdw, type="mapping", 
       labels =  as.integer(indices), col = 1,
       cex=font_size,
       main = "SARS-Cov-2: 0 Negativo, 1 Positivo")
}
dev.off()

