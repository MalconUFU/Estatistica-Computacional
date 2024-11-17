#Questão 1. Vovó Juju e seu neto Jorel estão apostando num jogo de cara ou coroa. Uma moeda é lançada: se sair cara, Vovó Juju recebe 1 real de Jorel; se sair coroa, Vovó Juju paga 1 real para seu neto. No início do jogo, Vovó Juju tem 18 reais e Jorel tem 7 reais. O jogo termina quando algum dos jogadores perde todo o seu dinheiro. Simule uma partida deste jogo. A simulação deve retornar o nome do vencedor do jogo. Agora, simule 10 mil vezes este experimento e calcule a média de vezes em que Jorel ganhou e a média de vezes que Vovó Juju ganhou. Interprete o resultado. KNN, arvore de decisão, floresta aleatória

moeda <- c("cara", "coroa")
juju <- 18
jorel <- 7

sorteio <- sample(moeda, size = 1)
sorteio

if(sorteio == "cara"){
  
  juju <- juju + 1
  jorel <- jorel - 1
  
}else{
  
  juju <- juju - 1
  jorel <- jorel + 1
  
}

while(juju != 0 & jorel != 0){
  
  sorteio <- sample(moeda, size = 1)
  
  if(sorteio == "cara"){
    
    juju <- juju + 1
    jorel <- jorel - 1
    
  }
  
  else{
    
    juju <- juju - 1
    jorel <- jorel + 1
    
  }
  
}

if(jorel == 0){
  print("juju")
}else {
  print("jorel")
}

resultados <- c()

for(j in 1:10000){
  
  juju <- 10
  jorel <- 10
  
  while(juju != 0 & jorel != 0){
    
    sorteio <- sample(moeda, size = 1)
    
    if(sorteio == "cara"){
      
      juju <- juju + 1
      jorel <- jorel - 1
      
    }
    
    else{
      
      juju <- juju - 1
      jorel <- jorel + 1
      
    }
    
  }
  
  if(jorel == 0){
    
    resultados <- c(resultados, "juju")
    
  }else {
    
    resultados <- c(resultados, "jorel")
    
  }
  
}

resultados
mean(resultados == "juju")
mean(resultados == "jorel")

#Questão 2. Uma urna contém bilhetes numerados de 1 até 30, todos do mesmo tamanho. Considere o seguinte experimento: retirar um bilhete da urna, registrar o resultado e devolver o bilhete para a urna. Você continuará a sortear bilhetes até que todos os números sejam retirados. Seja N o número de sorteios que você precisou realizar até que todos os números fossem registrados. Simule este experimento; a simulação deve retornar o valor N. Agora, simule 10 mil vezes este experimento. Calcule a média dos resultados obtidos e interprete o valor encontrado.

figurinhas <- 1:30
album <- sample(figurinhas, size = 1)

while(length(unique(album)) < 30){
  
  album <- c(album, sample(figurinhas, size = 1))
  
}

length(album)

resultados <- c()

for(j in 1:10000){
  
  album <- sample(figurinhas, size = 1)
  
  while(length(unique(album)) < 30){
    
    album <- c(album, sample(figurinhas, size = 1))
    
  }
  
  resultados[j] <- length(album)
  
}

resultados
mean(resultados)

#3

#a

library(ggplot2)

chicago <- read.csv(file = "chicago.csv", header = TRUE, sep = ",")

str(chicago)

chicago <- chicago[,-1]
chicago$season <- as.factor(chicago$season)
chicago$year <- as.factor(chicago$year)

sum(chicago$cvd)
ggplot(data = chicago, aes(x = season, y = cvd)) + geom_col() + theme_minimal()


#b

ggplot(data = chicago, aes(x = year, y = resp)) + geom_col() + theme_minimal()

#c

ggplot(data = chicago, aes(x = season, y = temp)) + geom_boxplot() + theme_minimal()

ggplot(data = chicago, aes(x = season, y = rhum)) + geom_boxplot() + theme_minimal()

#d)

ggplot(data = chicago, aes(x = time, y = temp, col = season)) + geom_point() + theme_minimal()

#4

#a

papagaio <- read.table(file = "papagaio.txt", header = TRUE, sep = ",")

head(papagaio)
tail(papagaio)
str(papagaio)
summary(papagaio)

#b

papagaio$especie <- as.factor(papagaio$especie)

library(ggplot2)

ggplot(data = papagaio, aes(x = papagaio$especie, fill = papagaio$especie)) + geom_bar() + theme_classic() + labs(title = "Espécies de Papagaio", x = "Especie", y = "Quantidade", fill = "Especies")

#c

ggplot(data = papagaio, aes(x = papagaio$especie, y = papagaio$peso, fill = papagaio$especie)) + geom_boxplot() + labs(title = "Pesos de Papagaios de Diferentes espécies", x = "Especies", y = "Peso (gramas)", fill = "Especies") + theme_classic()

ggplot(data = papagaio, aes(x = papagaio$especie, y = papagaio$envergadura, fill = papagaio$especie)) + geom_boxplot() + labs(title = "Envergadura de Papagaios de Diferentes espécies", x = "Especies", y = "Envergadura (cm)", fill = "Especies") + theme_classic()

ggplot(data = papagaio, aes(x = papagaio$especie, y = papagaio$tamanho, fill = papagaio$especie)) + geom_boxplot() + labs(title = "Pesos de Papagaios de Diferentes espécies", x = "Especies", y = "Tamanho (cm)", fill = "Especies") + theme_classic()

ggplot(data = papagaio, aes(x = papagaio$tamanho, y = papagaio$peso, col = papagaio$especie)) + geom_point() + labs(title = "Pesos de Papagaios de Diferentes espécies", x = "Especies", y = "Tamanho (cm)", fill = "Especies") + theme_classic()

#d

n <- round(0.8 * nrow(papagaio))

indices <- sample(1:nrow(papagaio), size = n, replace = FALSE)

treino <- papagaio[indices, ]
teste <- papagaio[-indices, ]

library(rpart)
library(rpart.plot)

modelo.arvore <- rpart(formula = especie~., data = treino, method = "class")

rpart.plot(modelo.arvore, extra = 101)

previsao <- predict(modelo.arvore, newdata = teste, type = "class")

mean(previsao == teste$especie)

#e

n <- round(0.8 * nrow(papagaio))

indices <- sample(1:nrow(papagaio), size = n, replace = FALSE)

treino <- papagaio[indices,] 
teste <- papagaio[-indices,]

treino_padronizado <- scale(treino[, c("peso", "tamanho", "envergadura")])
teste_padronizado <- scale(teste[, c("peso", "tamanho", "envergadura")])

classe_treino <- treino$especie
classe_teste <- teste$especie

library(class)

modelo.knn <- knn(treino_padronizado, teste_padronizado, cl = classe_treino, k = 100)

acuracia_knn <- mean(modelo.knn == classe_teste)
print(acuracia_knn)

library(caret)
confusionMatrix(table(modelo.knn, classe_teste))
