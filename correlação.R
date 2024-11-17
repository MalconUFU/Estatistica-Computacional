#correlação de duas variáveis 

data(iris)

library(ggplot2)

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point()

cor(iris$Petal.Length, iris$Petal.Width)

cor(iris[,-5])

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point() + facet_wrap(~iris$Species)

setosa <- iris[iris$Species == "setosa",]

cor(setosa$Petal.Length, setosa$Petal.Width)

femur <- read.csv("femur.csv", header = TRUE, sep = ",")

femur <- femur[,-1]
femur$genero <- as.factor(femur$genero)

homem <- femur[femur$genero == "Male",]
mulher <- femur[femur$genero == "Female",]

cor(homem$altura, homem$femur)
cor(mulher$altura, mulher$femur)

ggplot(data = homem, aes(x = femur, y = altura)) + geom_point()
ggplot(data = mulher, aes(x = femur, y = altura)) + geom_point()
ggplot(data = femur, aes(x = femur, y = altura)) + geom_point() + facet_wrap(~genero)

modelo_linear <- lm(data = homem, formula = altura ~ femur)
modelo_linear <- lm(data = mulher, formula = altura ~ femur)

modelo_linear
#altura = 2.61 * femur + 44.20
#altura = 2.019 * femur + 67.579

summary(homem$femur)
summary(mulher$femur)

grilo <- read.table(file = "grilo.txt", header = TRUE, sep = ",")

str(grilo)

ggplot(data = grilo, aes(x = grilo$frequencia)) + geom_histogram(bins = 10, col = "white") + labs(title = "Temperatura e frequência do cantar de grilos", x = "Temperatura", y = "Frequência")

cor(grilo)
cor(grilo$temperatura, grilo$frequencia)

#reta de regressão linear simples

ggplot(data = grilo, aes(x = grilo$temperatura, y = grilo$frequencia)) + geom_point() + labs(title = "Temperatura e frequência do cantar de grilos", x = "Temperatura", y = "Frequência") + theme_minimal() + geom_smooth(method = "lm", se = FALSE)

modelo_linear <- lm(data = grilo, formula = frequencia ~ temperatura)
modelo_linear # frequencia = 6.4812 + 0.3812 * temperatura

summary(grilo$temperatura)

w <- data.frame(temperatura = c(21, 23.6, 30.9))

predict(modelo_linear, newdata = w)

library(palmerpenguins)
library(dplyr)

pinguins <- penguins

pinguins <- na.omit(pinguins)

#pinguins <- pinguins[,-c(1, 2, 7, 8)]

cor(pinguins[,3:6])

ggplot(data = pinguins, aes(x = pinguins$flipper_length_mm, y = pinguins$body_mass_g, color = pinguins$species)) + geom_point() + labs(title = "Pinguins", x = "Asa", y = "Peso") + theme_minimal() + geom_smooth(method = "lm")

pinguins |> filter(species == "Chinstrap") |> select(flipper_length_mm, body_mass_g) |> cor()

modelo2 <- lm(data = pinguins, formula = pinguins$flipper_length_mm ~ pinguins$body_mass_g + pinguins$bill_length_mm)
modelo2
summary(modelo2)
