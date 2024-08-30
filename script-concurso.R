library(tidyverse)
# Caminho dos dados
url<-"https://raw.githubusercontent.com/arpanosso/experimentacao-agricola-unesp-fcav/master/img/aula05/mandioca.txt"

# Lendo o arquivo de dados
dados<-read.table(url, h = TRUE)
trat <- dados %>% pull(Trat) %>% as_factor()
y <- dados %>%  pull(Y)
ExpDes.pt::dic(trat, y, mcomp = "tukey")

### Checagem dos resíduos
# Realizando a ANOVA
mod <- aov(y ~trat)
anova(mod)

# Guardando os resíduos e valores preditos do modelo
rs <- rstudent(mod)
yp <- predict(mod)
dados <- dados %>%
  mutate(rs = rstudent(mod),
         yp = predict(mod))
head(dados)


### Histograma dos resíduos
dados %>%
  ggplot(aes(x=rs,y=..density..)) +
  geom_histogram(bins=10,col="black",fill="gray") +
  theme_bw()

### Gráfico Quantil-Quantil
dados %>%
  ggplot(aes(sample=rs)) +
  stat_qq() +
  stat_qq_line(color="blue") +
  theme_bw()

### Testes de Normalidade
shapiro.test(rs)
rs %>% nortest::ad.test()
rs %>% nortest::lillie.test()
rs %>% nortest::cvm.test()

### Testando a Homocedasticidade
lawstat::levene.test(y,trat)
lawstat::levene.test(y,trat, location = "mean")

dados %>%
  ggplot(aes(x=Trat,y=Y)) +
  geom_boxplot(fill="orange") +
  labs(x="Tratamentos") +
  theme_bw()

### Checagem de "outliers"
dados %>%
  ggplot(aes(x=yp,y=rs)) +
  geom_point(col="blue",size=6)+
  theme(axis.text.y = element_text(size=rel(2)),
        axis.text.x = element_text(size=rel(2)))
