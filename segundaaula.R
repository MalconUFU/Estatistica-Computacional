#CRIANDO VETORES

x1 <- c(3, 10, 78)

length(x1)

sum(x1)

mean(x1)

c(1, 6, "teste")

x1[c(2, 3)]

x2 <- c(x1, 67, 90)

x2 < 70

sum(x2 < 70)
x2[x2 < 70]

x2 != 10

TRUE | FALSE
TRUE & FALSE

?sample

dado1 <- sample(x = 1:6, size = 1000000, replace = TRUE)

mean(dado1 == 3)

dado1[102]

table(dado1)

barplot(table(dado1))
