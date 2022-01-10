url <- "https://raw.githubusercontent.com/arpanosso/experimentacao-agricola-unesp-fcav/master/data/dados_prod_cana.txt"

# Entrando com os dados
dados <- read.table(url, h= TRUE)

# Checando o 6 primeiros registros do banco de dados
head(dados)

trat <- as.factor(dados$Tratamento)
bloco <- as.factor(dados$Bloco)
resp <- dados$Producao

# ANOVA COM t de student
library(ExpDes.pt)
dbc(trat, bloco, resp, mcomp="lsd")

# ANOVA COM O Tukey
dbc(trat, bloco, resp, mcomp="tukey")


# ANOVA COM O Duncan
dbc(trat, bloco, resp, mcomp="duncan")
