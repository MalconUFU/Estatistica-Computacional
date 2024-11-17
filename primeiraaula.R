#operações básicas

1 + 1 #adição
1 - 10 #subtração
2 * 87 #multiplicação
34 / 21 #divisão
3**6 #potência 3^6
10 %% 3 #resto

a <- 123 * 98 #Criando um objeto em erro
12 + 1245 -> b

class(a) #Saber a classe de um objeto

w <- "Furiosa"

y <- TRUE #TRUE = 1 / FALSE = 0

z <- c(TRUE, FALSE, FALSE, FALSE)

#VETORES

x <- c(23, 10, 45)

d <- c(x, 45)
d + c(TRUE, FALSE)
# 23 10 45 45
# T  F. T  F.

#x1 <- c(23, 21, 24)
#d + x1 -> (46, 31, 69, 68)

d[c(2,3,3)]

x < 30
sum(x < 30) #SOMA DOS VALORES LÓGICOs (1 + 1 + 0 = 2)

x[x < 30] 
#x[x < 30] --> x[TRUE TRUE FALSE] --> 23 10 ??
#ATENÇÃO (RETORNAR OS VALORES DENTRO DO VETOR)

#x[sum(x < 30)] --> x[sum(TRUE TRUE FALSE)] --> x[sum(1 1 0)] --> x[2] --> 10
#OPERAÇÕES LÓGICAS -> RESULTADOS LÓGICOS (<, >, ==, &&, ||) (TRUE, FALSE)

which(x < 30) #EXIBIR POSIÇÕES

#sum(x[which(x < 30)]) --> sum(x[c(1, 2)]) --> sum(23, 10) --> 33

c(12, 34, "cruzeiro") #Converte as posições para a mesma classe

c(14, 52, 1.6)

p <- 23:10345
p[8]

?sample

set.seed(1805)

dado1 <- sample(x = 1:6, size = 10000, replace = TRUE, prob = NULL)
dado1

sum(dado1 == 3) / 10000
mean(dado1 == 3)

dado2 <- sample(x = 1:6, size = 10000, replace = TRUE, prob = NULL)

soma <- dado1 + dado2

soma[1:10]
mean(soma == 3)


