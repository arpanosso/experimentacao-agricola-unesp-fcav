A <- c(1470, 	1920, 	2340, 	2100, 	1920 ,1480)
B <- c (3260, 	3990, 	4050, 	3420, 	3510, 	3880, 	3550, 	3660)


# Teste de variâncias entre duas populações
var.test(A,B) ########

# Teste para média de duas populações
mean(A)
mean(B)


# Variância Iguais (Homocedasticidade)
t.test(A,B, var.equal = TRUE) ##########

# Variâncias diferentes (Heterocedasticidade)
t.test(A,B, var.equal = FALSE)
