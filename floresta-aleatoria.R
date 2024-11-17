#FLORESTA ALEATÓRIA (VÁRIOS MODELOS DE ÁRVORE)
#BIBLIOTECA: rpart, randomForest

library(randomForest) #BIBLIOTECA FLORESTA ALEATÓRIA
library(rpart) #BIBLIOTECA DE ÁRVORE

dados <- read.csv("C:/Users/Dell/Documents/EC/cancer.csv", header = TRUE)
str(dados)
dados$diagnosis <- as.factor(dados$diagnosis)

n <- round(0.8 * nrow(dados))

set.seed(1731)

indices_treino <- sample(1:nrow(dados), size = n, replace = FALSE)
indices_treino

treino <- dados[indices_treino, ]
teste <- dados[-indices_treino, ]

arvore <- rpart(formula = diagnosis ~., data = treino, method = "class")
# ~ em função de, . qualquer coisa

previsao.arvore <- predict(arvore, newdata = teste, type = "class")

mean(previsao.arvore == teste$diagnosis)

floresta <- randomForest( formula = diagnosis ~., data = treino, ntree = 200)
floresta

previsao.floresta <- predict(floresta, newdata = teste, type = "class")
previsao.floresta

mean(previsao.floresta == teste$diagnosis)


