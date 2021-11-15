# Ricardo Abreu n.º2102128

#Actividade 1
#1. Obtenha os resultados (aproximados à quarta décimal), das seguintes operações:


round((11^-4)-(1/3), digits=4) # a) ou round(11^-4-1/3, digits=4)

round((pi/5)*3, digits=4) # b) ou round(pi/5*3, digits=4)

round(sin(pi/4)+cos(pi/5), digits=4) # c)

round((exp(1)^-2)+tan(pi/3)+sqrt(10.5), digits=4) # d)

#2. Considere uma amostra x constituída pelos algarismos do seu número de estudante e obtenha:

#Número de estudante 2102128

x<-c(2,1,0,2,1,2,8) # transformação do número de estudante num vetor (amostra)

max(x) # Valor Máximo
min(x) # Valor Mínimo
sum(x) # Soma de todos os algarismos
mean(x) # Média aritmética dos valores no vetor x
median(x,na.rm = FALSE) #Médiana  dos valores no vetor x. como não existe valores omissos podia ser median(x)
ai<-max(x)-min(x) # Amplitude amostral é a diferença entre o valor máximo e valor minimo
ai
var(x) # Variancia amostral
prod(x,na.rm = FALSE) # Produto dos elementos do vetor x. como não existe valores omissos podia ser prod(x)
length(x) #Contagem do número de elementos do vetor x
S<-cumsum(x) # vetor S representa um vetor da soma acumulada de x.
C<-cumprod(x) # vetor C representa um vetor do produto acumulada dos valores de  x.
L<-sort(x,decreasing=F) # vetor L representa um vetor dos valores ordenados de  x ordenados de forma crescente.

# 3. Crie um vetor linha n~ao numérico contendo o resultado de 10 lançamentos de uma moeda: y = (cara, coroa, coroa, cara, cara, cara, coroa, cara, coroa, cara)

y<-c("cara", "coroa", "coroa", "cara", "cara", "cara", "coroa", "cara", "coroa", "cara")

y[3] #como o vetor y é constituido por dez elementos sequenciais podemos acessar a um determinado elemento atraves de um indice entre chavetas.

length(y[y=="cara"]) # a expressão y[y=="cara"] seleciona a partir do vetor y todos os elementos iguais a "cara" e a função lenght conta os lementos presentes.

# 4

v<-c("fumador", "não-fumador","não-fumador","não-fumador","fumador","fumador","não-fumador","não-fumador","não-fumador", "fumador","fumador","fumador","não-fumador","não-fumador","não-fumador")
w<-c("masculino","masculino","feminino","feminino","masculino","masculino","masculino", "feminino","masculino","feminino","feminino","masculino","masculino","feminino","feminino")

tabela_v<-table(v) # a) tabela de frequencia do vetor v
tabela_v

tabela_vw<-table(v,w) # tabela cruzada de frequencias do vetor v e w
tabela_vw

# 5 Crie as seguintes sequências de números:

seq_a<-1:9
seq_b<-seq(0,50,6.25)*-1
seq_c<-seq(from=100, to=10, by=-9.9)
seq_d<-rep(1:5,4)

# 6 contrua a matriz

B<-rbind(c(15,2,65,19),c(12,14,35,23),c(18,25,27,16))

B[3,2]      # a)
mean(B[3,]) # b)
var(B[,4])  # c)
C<-B*5      # d)
A<-B%*%t(C) # e)


