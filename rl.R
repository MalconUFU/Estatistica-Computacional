#importe o arquivo femur.csv
#crie um conjunto apenas com os homens
#calcule o coeficiente de correlação entre o altura e femur
#determine a reta de regressão linear

library(ggplot2)

#passo 01: analisar a correlação
femur <- read.csv("femur.csv")

femur <- femur[,-1]

femur$genero <- as.factor(femur$genero)

homens <- femur[femur$genero == "Male",]

cor(homens$femur, homens$altura)

ggplot(data = homens, aes(x = femur, y = altura)) + geom_point() + geom_smooth(method = "lm")

#cor.test: teste de hipoteses para verificar se o coeficiente de correlação é 0

#H0: coeficiente de correlação é zero
#Ha: coeficiente de correlação não é zero

#quando p-valor for muito pequeno, vamos negar a hipotese nula H0
cor.test(homens$altura, homens$femur)

#passo 02:

#vamos determinar a reta de regressão e verificar se é um bom modelo

modelo <- lm(data = homens, formula = altura ~ femur)
modelo

summary(modelo)

#passo 03:

#analisar os resíduos (isso é, analisar os erros)

#modelo$residuals

hist(modelo$residuals)

#vamos realizar um teste de hipóteses para verificar se os residuos seguem uma distribuição normal

#descobrir se o modelo é bom
#H0: os residuos seguem um distribuição normal
#Ha: os residuos não seguem um distribuição normal

shapiro.test(modelo$residuals)

#ABAIXO DE 5%, REJEITA-SE H0
