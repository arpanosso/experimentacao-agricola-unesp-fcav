# Resolução Lista 06
# Carregando os pacotes
library(agricolae)
library(tidyverse)
library(nortest)
library(lawstat)
library(ExpDes.pt)

## Entrada de Dados
### Definição da URL na internet do banco de dados
caminho <- "https://raw.githubusercontent.com/arpanosso/ExpAgr_2020/master/dados/nNinfas.txt"

### Entrada de dados
dados <- read.table(caminho,h=TRUE,sep="\t")

### Observando os 6 primeiros registro
head(dados)

# Gráfico Exploratório
dados %>% 
  ggplot(aes(x=Trat, y=nNinfas,fill=as.factor(Trat))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x="Tratamentos",y=)

# Análise de variância utilizando as funções do pacote básico do R
# Criando o vetor de tratamentos e da variável resposta.
trat <- as.factor(dados$Trat)
nNinfas <- dados$nNinfas

# Definindo o modelo para o DIC
mod <- aov(nNinfas ~ trat)

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
levene.test(nNinfas,trat)
levene.test(nNinfas,trat,location = "mean")

# Realizando o teste de Tukey
tukey<-HSD.test(mod,"trat",group=TRUE,console = TRUE)

# Calculando o CV
ep<-sqrt(deviance(mod)/df.residual(mod))
m <- mean(nNinfas)
CV <- 100*ep/m

# Utilizando a função dic do pacote ExpDes.pt
dic(trat,nNinfas,mcomp = "tukey")

