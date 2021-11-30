# Criando o vetor de dados
x <- c(5, 3, 2, 4, 3)

# Calculando o tamanho do vetor
n <- length(x)

# Calculando a Soma
G <- sum(x)

# Calculando a Média aritmética
m <- mean(x)

# Calculando os desvios
d <- x - m
d
round(sum(d))

# Cálculo da Variância
## Primeiro a Soma de Quadrados
SQD_1 <- sum((d)^2)
SQD_2 <- sum(x^2) - (sum(x))^2/n

## Calculando a variância
SQD_1 / (n -1)
var(x)

# Calcuando o Desvio Padrão
sd(x)

# Calculando o erro padrão da média
sd(x)/sqrt(5)

# Calculando o Coeficiente Variação
100*sd(x)/mean(x)


# Exemplo da lista, entrando com dados
pcana <- c(110.6, 119.5, 120.1, ..., 149.8)













