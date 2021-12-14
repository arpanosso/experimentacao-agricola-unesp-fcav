# Resolução Lista 07
# Carregando os pacotes
library(agricolae)
library(tidyverse)
library(nortest)
library(lawstat)
library(ExpDes.pt)

## Entrada de Dados
### Definição da URL na internet do banco de dados
caminho <- "https://raw.githubusercontent.com/arpanosso/ExpAgr_2020/master/dados/Batata.txt"

### Entrada de dados
dados <- read.table(caminho,h=TRUE,sep="\t")

### Observando os 6 primeiros registro
head(dados)

# Filtrando os dados perdidos
dados<- dados %>% 
  filter(!is.na(Y))
dados

# Gráfico Exploratório
dados %>% 
  ggplot(aes(x=Trat, y=Y,fill=as.factor(Trat))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x="Tratamentos",y="Produção (t/ha)",fill="Tratamentos")

# Análise de variância utilizando as funções do pacote básico do R
# Criando o vetor de tratamentos e da variável resposta.
trat <- as.factor(dados$Trat)
prod <- dados$Y

# Definindo o modelo para o DIC
mod <- aov(prod ~ trat)

# Quadro da análise de variância
anova(mod)

# Testando a Normalidade dos resíduos:
# a) Extrair os resíduos do modelo
rs <- rstudent(mod)

# b) Testando a normalidade dos resíduos
shapiro.test(rs)
cvm.test(rs)
lillie.test(rs)
ad.test(rs)

# c) Testando a homocedasticidade (Homogeneidade das variâncias)
levene.test(prod,trat)
levene.test(prod,trat,location = "mean")

# Realizando o teste de Tukey
tukey<-HSD.test(mod,"trat",group=TRUE,console = TRUE)

# Calculando o CV
ep<-sqrt(deviance(mod)/df.residual(mod))
m <- mean(prod)
CV <- 100*ep/m

# Utilizando a função dic do pacote ExpDes.pt
dic(trat,prod,mcomp = "tukey",sigT = 0.01)

# Test t para comparar contraste
m <- tapply(prod,trat,mean,na.rm=TRUE)
r <- tapply(prod,trat, .nac<-function(x) {x<-na.omit(x);length(x)})
QMRes <- deviance(mod)/df.residual(mod)
glRes <- df.residual(mod)


# (B72 + B1_52) vs Kennebec
c1<-c(0,1,1,-2,0) # atribua os coeficientes de acordo com a ordem das médias
(Y1=sum(m*c1))
(tobs=Y1/sqrt(sum(c1^2/r)*QMRes))
(tc5=qt(1-0.05/2,glRes))
(tc1=qt(1-0.01/2,glRes))

# Construção do Gráfico para interpretação
curve(dt(x,glRes),-30,9,xlab="t",ylab="D(t)",las=1)
abline(v=0)
abline(v=c(tc5,-tc5,tc1,-tc1),col=2,lty=2)
abline(v=tobs,col=4,lty=1)
text(2.8,.1,expression(paste(alpha,"=2.5%")))
text(4,.02,expression(paste(alpha,"=0.5%")))
text(-2.8,.1,expression(paste(alpha,"=2.5%")))
text(-4,.02,expression(paste(alpha,"=0.5%")))
text(tobs,.05,"tobs")

