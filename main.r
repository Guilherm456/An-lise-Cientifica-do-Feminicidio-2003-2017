#leitura dos arquivos
arquivogeral <- read.csv2("Artigo.csv")
arquivo <- read.csv2("Artigo2.csv")

sink("dadosboxplot.txt") #cria um arquivo com os dados do boxplot


#Cria boxplot
colnames(arquivogeral)[2:16] <- c(2003:2017)
box<-boxplot(arquivogeral[-28,-c(1,17)],ylab="Número de ocorrências",
        xlab="Ano") #faz o boxplot com legendas
print(box$stats) #imprime os dados do boxplot
  
par(mar=c(5.1, 4.1, 4.1, 9), xpd=TRUE) #define as margens


#cria um gráfico de comparações 
matplot(c(2003:2017),
  t(arquivogeral[-28,2:16]),
  type="b",
  pch=c(1:27),
  col=c(1:27),
  xlab="Anos",
  ylab="Número de ocorrências")

legend("topright",
  inset=c(-0.3,-0.1), 
  legend=arquivogeral[-28,1], 
  pch = 1:27,
  title="Estados",
  col=1:27,
  cex=0.7)

# Vai criar um PDF com a regressão de cada Estado
pdf(file="Imagem.pdf")
par(mfrow=c(3,4)) #define o número de linha e colunas
j<-2
sink("arquivos.txt")
for(i in 2:28)
{
  modelo <- lm(arquivo[,i]~arquivo$X)

  #imprime os dados em um arquivo
  print(colnames(arquivo[j]))
  print(summary(modelo)$coefficients[,1]) #imprime os coeficientes
  cat("R-Squared: ", summary(modelo)$r.squared) #imprime o r²
  cat("\nAdjusted R-Squared: ", summary(modelo)$adj.r.squared) #imprime o r² ajustado
  cat("\n\n====================\n\n")


  if(summary(modelo)$adj.r.squared>=0.7) #caso seja maior que 0,7
  {

    #cria o gráfico com a reta da tendencia
    plot(
      arquivo[,i]~arquivo$X
      ,col="red"
      ,pch=16
      ,type='l'
      ,main = colnames(arquivo[j])
      # ,cex.main=3
      ,xlab="Anos"
      ,ylab="Número de mortes femininas"
    )
    curve(
      coef(modelo)[1] + coef(modelo)[2]*x
      ,add=T
      ,col="blue"
    )
    
  }
  j=j+1

}
dev.off() #fecha arquivo

j<-2

#Vai criar um PDF com a regressão de cada Estado
pdf(file="R2MenorQue70.pdf")
par(mfrow=c(3,4))

for(i in 2:28)
{
  modelo <- lm(arquivo[,i]~arquivo$X)
  if(summary(modelo)$adj.r.square<0.7)
  {
    plot(
      arquivo[,i]~arquivo$X
      ,col="red"
      ,pch=16
      ,type='l'
      ,main = colnames(arquivo[j])
      # ,cex.main=3
      ,xlab="Anos"
      ,ylab="Número de mortes femininas"
    )
  }

  j=j+1;
}


dev.off()
sink()
