#0.9 e 0.1

populacao <- sample(x = 0:1, size = 100000, replace = TRUE, prob = c(0.9, 0.1))

amostra <- sample(x = dados, size = 200, replace = TRUE)

mean(amostra)

?prop.test

prop.test(x = sum(amostra == 1), n = 200, p = 0.1, alternative = "two.sided")

library(Stat2Data)
library(ggplot2)

ggplot(data = Oysters, aes(x = TwoD, y = Volume)) + geom_point() + labs(title = "Relação imagem e volume de ostras", x = "Pixels", y = "Volume") + theme_minimal() + geom_smooth(method = "lm", se = FALSE)

cor(Oysters$TwoD, Oysters$Volume)
cor.test(Oysters$TwoD, Oysters$Volume)

modelo_linear <- lm(data = Oysters, formula = Volume ~ TwoD)
modelo_linear

summary(modelo_linear)

modelo_linear$residuals
hist(modelo_linear$residuals)

shapiro.test(modelo_linear$residuals)
