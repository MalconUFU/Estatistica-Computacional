#REVISÃO P1

#1- a)

numeros <- rep(1:13, times = 4)
numeros

cores <- c(rep("branca", times = 13), 
           rep("azul", times = 13), 
           rep("vermelho", times = 13), 
           rep("marrom", times = 13))

numeros
cores

id <- 1:52
baralho <- data.frame(id = 1:52, numeros, cores)

sorteio <- sample(id, size = 3, replace = FALSE)

sum(baralho$cores[sorteio] == "azul")
resultados <- c()

for(k in 1:100000){
  
  sorteio <- sample(id, size = 3, replace = FALSE)
  resultados[k] <- sum(baralho$cores[sorteio] == "azul")
  
}

resultados
mean(resultados == 2)

paste0(1:13, "a")
paste0(1:13, "b")
paste0(1:13, "v")
paste0(1:13, "m")

baralho2 <- c(paste0(1:13, "a"), paste0(1:13, "b"), paste0(1:13, "v"), paste0(1:13, "m"))

sorteio <- sample(baralho2, size = 3, replace = FALSE)
sorteio
library(stringr)

as.numeric(str_extract(sorteio, "\\d+"))
str_replace(sorteio, "\\d+", ' ')

#1 - b)

contador_cartas <- 0
contador_7 <- 0

while(contador_7 < 4){
  
  sorteio <- sample(numeros, size = 1)
  
  if(sorteio == 7){
    
    contador_7 <- contador_7 + 1
    
  }
  
  contador_cartas <- contador_cartas + 1
  
}

contador_cartas

resultados <- c()

for(j in 1:10000){
  
  contador_cartas <- 0
  contador_7 <- 0
  
  while(contador_7 < 4){
    
    sorteio <- sample(numeros, size = 1)
    
    if(sorteio == 7){
      
      contador_7 <- contador_7 + 1
      
    }
    
    contador_cartas <- contador_cartas + 1
    
  }
  
  resultados[j] <- contador_cartas
  
}

mean(resultados)

#2- a)

churn <- read.table("churn.txt", header = TRUE, sep = ";")

str(churn)

churn <- churn[, -c(1, 2, 3)]

churn$Gender <- as.factor(churn$Gender)
churn$Geography <- as.factor(churn$Geography)
churn$HasCrCard <- as.factor(churn$HasCrCard)
churn$IsActiveMember <- as.factor(churn$IsActiveMember)
churn$Exited <- as.factor(churn$Exited)

n <- round(0.75 * nrow(churn))
indices_treino <- sample(1:nrow(churn), size = n, replace = FALSE)

treino <- churn[indices_treino,]
teste <- churn[-indices_treino,]

library(rpart)
library(rpart.plot)

modelo <- rpart(formula = Exited~., data = treino, method = "class")
modelo
rpart.plot(modelo, extra = 101)

previsao <- predict(modelo, newdata = teste, type = "class")
mean(previsao == teste$Exited)
table(teste$Exited, previsao)
