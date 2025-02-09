#ESTATISTICA COMPUTACIONAL ACTIVIDADE 1 RICARDO ABREU (2102128)

install.packages(ISwR)
library(ISwR)

set.seed(2102128)

#An�lise estatistica e gr�fica aos dados do dataset VITCAP2

vitcap2
summary(vitcap2)

#Tipo de exposi��o
texp<-table(vitcap2$group) 
lexp<-c("Exposto <10", "Exposto >10", "N�o exposto") #Etiquetas do tipo de exposi��o (labels)
percentlexp<- round(100*texp/sum(texp), 1) #valores percentuais de cada tipo de exposi��o
pielabels<-paste(percentlexp, "%", sep="") #adicionar o simbolo de %

pie(texp, labels = pielabels, col = c("yellow","red","green"), main = "Tipo de exposi��o ao C�dmio") #constru��o do gr�fico circular ("queijo") com as respetivas cores
legend("topright", lexp, cex=0.8, fill=c("yellow","red","green")) #adicionar a legenda ao gr�fico circular

#histograma � vari�vel idade
h_idade<-hist(vitcap2$age, 
              main = "Histograma de idades dos trabalhadores", 
              xlab = "Idades",ylab = "frequ�ncia", col = "wheat3") 
text(h_idade$mids,h_idade$counts,labels=h_idade$counts, adj=c(0.5, -0.5)) #introdu��o no histograma dos valores respectivos de frequencia

#Boxplot da Capacidade Vital
boxplot(vitcap2$vital.capacity,
        main = "Capacidade Vital dos trabalhadores",
        xlab = "Volume do pulmonar (ml)",
        ylab = "AR",
        col = "slateblue2",
        border = "black",
        horizontal = TRUE
        
)


bin_idade <- cut(vitcap2$age, breaks = 5) #dividir a idade em classes et�rias

binframe<-data.frame(bin_idade,vitcap2$vital.capacity) #agrupar as classes et�rias com a capacidade vital

factor(binframe$bin_idade) #transformar vari�vel classe et�ria em categ�rica

#boxplot classe et�ria por capacidade vital
statplot<- plot(factor(binframe$bin_idade),binframe$vitcap2.vital.capacity,
                xlab = "Classe Et�ria",  
                ylab = "Capacidade Vital",  
                main = "Capacidade Vital por grupo et�rio"
)

frameplot<-data.frame(statplot$stats, row.names = c("min","1�Q","mediana","3�Q","max" )) #transformar os resultados estatisticos da boxplot em tabela

colnames (frameplot)<- c("]18-27.4]","]27.4-36.8]","]36.8-46.2]","]46.2-55.6]","]55.6-65]") #classificar as colunas da tabela de estatisticas por classes et�rias

frameplot


#An�lise estatistica e gr�fica aos dados do dataset TLC

tlc
summary(tlc)


#histograma � vari�vel idade


tlc_idade<-hist(tlc$age, 
              main = "Histograma de idades dos trabalhadores", 
              xlab = "Idades",ylab = "frequ�ncia", col = "lightblue") 
text(tlc_idade$mids,tlc_idade$counts,labels=tlc_idade$counts, adj=c(0.5, -0.5)) #introdu��o no histograma dos valores respectivos de frequencia

#bloxplot �s vari�veis quantitativas

par(mfrow=c(1,2)) #dividir em dois a janela dos gr�ficos

boxplot(tlc$height~tlc$sex,
        ylab="Altura", xlab="Genero",
        main="Altura por genero",
        col=c("coral1","cadetblue"),
        names=c("Feminino","Masculino")
        )

boxplot(tlc$tlc~tlc$sex,
        ylab="TLC", xlab="Genero",
        main="TLC por genero",
        col=c("coral1","cadetblue"),
        names=c("Feminino","Masculino")
)



#Gr�fico de pontos. Rela��o entre altura e TLC (regress�o linear)

par(mfrow=c(1,1))

plot(tlc$height,tlc$tlc, 
     xlab = "Altura",
     ylab = "TLC", 
     main = "Rela��o Altura vs TLC", 
     pch=19, 
     cex=0.8,
     abline(lm(tlc$tlc~tlc$height),col="blue")
   
)


#An�lise estatistica e gr�fica aos dados do dataset SECHER

secher
summary(secher)

#Boxplot das vari�veis quantitativas

par(mfrow=c(2,1))

boxplot(secher[2:3], 
        horizontal = T,
        xlab="Diametro (mm)",
        ylab="Tipologia",
        cex.lab=1.3,
        cex.axis=0.6,
        main="medi��es ultrassonogr�ficas",
        col=c("green","blue"),
        names = c("bipariatal","abdominal"),
        notch = T,
        outpch = 19
        )

plot(secher[,1], type="h", ylab = "Peso", xlab = "observa��es", cex.lab=1.3,cex.axis=0.8)

#correla��es entre as vari�veis quantitativas

par(mfrow=c(1,1))
plot(secher[1:3])

#correla��es com reta de regress�o
par(mfrow=c(3,1))

plot(secher$bwt,secher$bpd, 
     xlab = "Peso",
     ylab = "diametro bipariatal", 
     main = "Rela��o peso vs diam. biparietal", 
     pch=19, 
     cex=0.8,
     abline(lm(secher$bpd~secher$bwt),col="blue")
)

plot(secher$bwt,secher$ad, 
     xlab = "Peso",
     ylab = "diametro abdominal", 
     main = "Rela��o peso vs diam. abdominal", 
     pch=19, 
     cex=0.8,
     abline(lm(secher$ad~secher$bwt),col="green")
)

plot(secher$bpd,secher$ad, 
     xlab = "diametro biparietal",
     ylab = "diametro abdominal", 
     main = "Rela��o biparietal vs diam. abdominal", 
     pch=19, 
     cex=0.8,
     abline(lm(secher$ad~secher$bpd),col="yellow")
)

#histograma e densidades

par(mfrow=c(2,2))

#histograma (abdominal)
hsecher_ad<-hist(secher$ad, freq=T, breaks=10, 
              main = "Histograma do diametro abdominal", 
              ylab="Frequ�ncia", xlab = "diametro (mm)",
              col="orange")

#curva da Normal do histograma (biparietal)
x<-seq(min(secher$ad),max(secher$ad),length=40) 
y<-dnorm(x,mean=mean(secher$ad),sd=sd(secher$ad)) 
y<- y*diff(hsecher_ad$mids[1:2])*length(secher$ad) 
lines(x, y, col="blue", lwd=2)

#histograma (biparietal)
hsecher_bpd<-hist(secher$bpd, freq=T, breaks=10, 
                  main = "Histograma do diametro biparietal", 
                  ylab="Frequ�ncia", xlab = "diametro (mm)",
                  col="brown")

#curva da Normal do histograma (biparietal)
x<-seq(min(secher$bpd),max(secher$bpd),length=40) 
y<-dnorm(x,mean=mean(secher$bpd),sd=sd(secher$bpd)) 
y<- y*diff(hsecher_bpd$mids[1:2])*length(secher$bpd) 
lines(x, y, col="blue", lwd=2)

#curva de densidade de kernel (abdominal)
dsecher_ad<-density(secher$ad)
plot(dsecher_ad, 
     ylab = "Densidade",
     main = "Densidade de Kernel do diametro abdominal")
polygon(dsecher_ad,col="orange", border="black")

#curva de densidade de kernel (biparietal)
dsecher_bpd<-density(secher$bpd)
plot(dsecher_bpd, 
     ylab = "Densidade",
     main = "Densidade de Kernel do diametro biparietal")
polygon(dsecher_bpd,col="brown", border="black")

