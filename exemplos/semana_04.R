# Entrada de dados (internet)
url <- "https://raw.githubusercontent.com/arpanosso/experimentacao-agricola-unesp-fcav/master/data/dados_prod_cana.txt"
dados <- read.table(url, h = TRUE)

# observar os 6 primeiros registros
head(dados)


# Entrada de dados (Excel)
library(readxl) # pacote
dados_prod_cana <- read_excel("C:/Users/Usuario/Downloads/dados_prod_cana.xlsx")
View(dados_prod_cana)


# Entrada de dados em TXT em um diretório do seu PC
dados <- read.table("C:\\Users\\Usuario\\Downloads\\dados_prod_cana.txt",
                    h=TRUE)
dados

# Análise de Variância
library(ExpDes.pt)

# Crianção de vetores para a análise
trat <- as.factor(dados$Tratamento)
bloco <- as.factor(dados$Bloco)
resp <- dados$Producaoresp


# Anova usando a função específica para delineamento em Blocos ao Acaso
dbc(trat, bloco, resp,
    mcomp = "tukey")



























