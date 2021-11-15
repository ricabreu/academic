#ESTATISTICA COMPUTACIONAL ACTIVIDADE 1 RICARDO ABREU (2102128)

install.packages(ISwR)
library(ISwR)

set.seed(2102128)

#Análise estatistica e gráfica aos dados do dataset VITCAP2

vitcap2
summary(vitcap2)

#Tipo de exposição
texp<-table(vitcap2$group) 
lexp<-c("Exposto <10", "Exposto >10", "Não exposto") #Etiquetas do tipo de exposição (labels)
percentlexp<- round(100*texp/sum(texp), 1) #valores percentuais de cada tipo de exposição
pielabels<-paste(percentlexp, "%", sep="") #adicionar o simbolo de %

pie(texp, labels = pielabels, col = c("yellow","red","green"), main = "Tipo de exposição ao Cádmio") #construção do gráfico circular ("queijo") com as respetivas cores
legend("topright", lexp, cex=0.8, fill=c("yellow","red","green")) #adicionar a legenda ao gráfico circular

#histograma à variável idade
h_idade<-hist(vitcap2$age, 
              main = "Histograma de idades dos trabalhadores", 
              xlab = "Idades",ylab = "frequência", col = "wheat3") 
text(h_idade$mids,h_idade$counts,labels=h_idade$counts, adj=c(0.5, -0.5)) #introdução no histograma dos valores respectivos de frequencia

#Boxplot da Capacidade Vital
boxplot(vitcap2$vital.capacity,
        main = "Capacidade Vital dos trabalhadores",
        xlab = "Volume do pulmonar (ml)",
        ylab = "AR",
        col = "slateblue2",
        border = "black",
        horizontal = TRUE
        
)


bin_idade <- cut(vitcap2$age, breaks = 5) #dividir a idade em classes etárias

binframe<-data.frame(bin_idade,vitcap2$vital.capacity) #agrupar as classes etárias com a capacidade vital

factor(binframe$bin_idade) #transformar variável classe etária em categórica

#boxplot classe etária por capacidade vital
statplot<- plot(factor(binframe$bin_idade),binframe$vitcap2.vital.capacity,
                xlab = "Classe Etária",  
                ylab = "Capacidade Vital",  
                main = "Capacidade Vital por grupo etário"
)

frameplot<-data.frame(statplot$stats, row.names = c("min","1ºQ","mediana","3ºQ","max" )) #transformar os resultados estatisticos da boxplot em tabela

colnames (frameplot)<- c("]18-27.4]","]27.4-36.8]","]36.8-46.2]","]46.2-55.6]","]55.6-65]") #classificar as colunas da tabela de estatisticas por classes etárias

frameplot


#Análise estatistica e gráfica aos dados do dataset TLC

tlc
summary(tlc)


#histograma à variável idade


tlc_idade<-hist(tlc$age, 
              main = "Histograma de idades dos trabalhadores", 
              xlab = "Idades",ylab = "frequência", col = "lightblue") 
text(tlc_idade$mids,tlc_idade$counts,labels=tlc_idade$counts, adj=c(0.5, -0.5)) #introdução no histograma dos valores respectivos de frequencia

#bloxplot Às variáveis quantitativas

par(mfrow=c(1,2)) #dividir em dois a janela dos gráficos

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



#Gráfico de pontos. Relação entre altura e TLC (regressão linear)

par(mfrow=c(1,1))

plot(tlc$height,tlc$tlc, 
     xlab = "Altura",
     ylab = "TLC", 
     main = "Relação Altura vs TLC", 
     pch=19, 
     cex=0.8,
     abline(lm(tlc$tlc~tlc$height),col="blue")
   
)


#Análise estatistica e gráfica aos dados do dataset SECHER

secher
summary(secher)

#Boxplot das variáveis quantitativas

par(mfrow=c(2,1))

boxplot(secher[2:3], 
        horizontal = T,
        xlab="Diametro (mm)",
        ylab="Tipologia",
        cex.lab=1.3,
        cex.axis=0.6,
        main="medições ultrassonográficas",
        col=c("green","blue"),
        names = c("bipariatal","abdominal"),
        notch = T,
        outpch = 19
        )

plot(secher[,1], type="h", ylab = "Peso", xlab = "observações", cex.lab=1.3,cex.axis=0.8)

#correlações entre as variáveis quantitativas

par(mfrow=c(1,1))
plot(secher[1:3])

#correlações com reta de regressão
par(mfrow=c(3,1))

plot(secher$bwt,secher$bpd, 
     xlab = "Peso",
     ylab = "diametro bipariatal", 
     main = "Relação peso vs diam. biparietal", 
     pch=19, 
     cex=0.8,
     abline(lm(secher$bpd~secher$bwt),col="blue")
)

plot(secher$bwt,secher$ad, 
     xlab = "Peso",
     ylab = "diametro abdominal", 
     main = "Relação peso vs diam. abdominal", 
     pch=19, 
     cex=0.8,
     abline(lm(secher$ad~secher$bwt),col="green")
)

plot(secher$bpd,secher$ad, 
     xlab = "diametro biparietal",
     ylab = "diametro abdominal", 
     main = "Relação biparietal vs diam. abdominal", 
     pch=19, 
     cex=0.8,
     abline(lm(secher$ad~secher$bpd),col="yellow")
)

#histograma e densidades

par(mfrow=c(2,2))

#histograma (abdominal)
hsecher_ad<-hist(secher$ad, freq=T, breaks=10, 
              main = "Histograma do diametro abdominal", 
              ylab="Frequência", xlab = "diametro (mm)",
              col="orange")

#curva da Normal do histograma (biparietal)
x<-seq(min(secher$ad),max(secher$ad),length=40) 
y<-dnorm(x,mean=mean(secher$ad),sd=sd(secher$ad)) 
y<- y*diff(hsecher_ad$mids[1:2])*length(secher$ad) 
lines(x, y, col="blue", lwd=2)

#histograma (biparietal)
hsecher_bpd<-hist(secher$bpd, freq=T, breaks=10, 
                  main = "Histograma do diametro biparietal", 
                  ylab="Frequência", xlab = "diametro (mm)",
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

