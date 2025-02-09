# Ricardo Abreu n.�2102128

#Actividade 1
#1. Obtenha os resultados (aproximados � quarta d�cimal), das seguintes opera��es:


round((11^-4)-(1/3), digits=4) # a) ou round(11^-4-1/3, digits=4)

round((pi/5)*3, digits=4) # b) ou round(pi/5*3, digits=4)

round(sin(pi/4)+cos(pi/5), digits=4) # c)

round((exp(1)^-2)+tan(pi/3)+sqrt(10.5), digits=4) # d)

#2. Considere uma amostra x constitu�da pelos algarismos do seu n�mero de estudante e obtenha:

#N�mero de estudante 2102128

x<-c(2,1,0,2,1,2,8) # transforma��o do n�mero de estudante num vetor (amostra)

max(x) # Valor M�ximo
min(x) # Valor M�nimo
sum(x) # Soma de todos os algarismos
mean(x) # M�dia aritm�tica dos valores no vetor x
median(x,na.rm = FALSE) #M�diana  dos valores no vetor x. como n�o existe valores omissos podia ser median(x)
ai<-max(x)-min(x) # Amplitude amostral � a diferen�a entre o valor m�ximo e valor minimo
ai
var(x) # Variancia amostral
prod(x,na.rm = FALSE) # Produto dos elementos do vetor x. como n�o existe valores omissos podia ser prod(x)
length(x) #Contagem do n�mero de elementos do vetor x
S<-cumsum(x) # vetor S representa um vetor da soma acumulada de x.
C<-cumprod(x) # vetor C representa um vetor do produto acumulada dos valores de  x.
L<-sort(x,decreasing=F) # vetor L representa um vetor dos valores ordenados de  x ordenados de forma crescente.

# 3. Crie um vetor linha n~ao num�rico contendo o resultado de 10 lan�amentos de uma moeda: y = (cara, coroa, coroa, cara, cara, cara, coroa, cara, coroa, cara)

y<-c("cara", "coroa", "coroa", "cara", "cara", "cara", "coroa", "cara", "coroa", "cara")

y[3] #como o vetor y � constituido por dez elementos sequenciais podemos acessar a um determinado elemento atraves de um indice entre chavetas.

length(y[y=="cara"]) # a express�o y[y=="cara"] seleciona a partir do vetor y todos os elementos iguais a "cara" e a fun��o lenght conta os lementos presentes.

# 4

v<-c("fumador", "n�o-fumador","n�o-fumador","n�o-fumador","fumador","fumador","n�o-fumador","n�o-fumador","n�o-fumador", "fumador","fumador","fumador","n�o-fumador","n�o-fumador","n�o-fumador")
w<-c("masculino","masculino","feminino","feminino","masculino","masculino","masculino", "feminino","masculino","feminino","feminino","masculino","masculino","feminino","feminino")

tabela_v<-table(v) # a) tabela de frequencia do vetor v
tabela_v

tabela_vw<-table(v,w) # tabela cruzada de frequencias do vetor v e w
tabela_vw

# 5 Crie as seguintes sequ�ncias de n�meros:

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


