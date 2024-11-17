#PROVA 1
#Malcon Rezende Rodrigues - 12211BCC034

#1 - a)

azuis <- rep("a", 13)
vermelhas <- rep("v", 13)
brancas <- rep("b", 13)
marrons <- rep("m", 13)

baralho <- c(azuis, vermelhas, brancas, marrons)

vezes <- numeric(100000)

for(k in 1:100000){
  
  mao <- sample(x = baralho, size = 3, replace = FALSE)
  
  i <- 1
  azul <- 0
  
  while(i <= 3){
    
    if(mao[i] == "a"){
      
      azul <- azul + 1
      
    }
    
    i <- i + 1
    
  }
  
  if(azul == 2){
    
    vezes[k] <- 1
    
  }else{
    
    vezes[k] <- 0 
    
  }
  
}

mean(vezes)

#O experimento retorno 0.13801, isso significa que, em média, 13% das vezes temos exatamente duas cartas azuis sorteadas na mão

sorteio_azul <- function(n){
  
  if(n < 1){
    
    return("n menor que 1")
    
  }
  else{
  
  azuis <- rep("a", 13)
  vermelhas <- rep("v", 13)
  brancas <- rep("b", 13)
  marrons <- rep("m", 13)
  
  baralho <- c(azuis, vermelhas, brancas, marrons)
  
  vezes <- numeric(n)
  
  for(k in 1:n){
    
    mao <- sample(x = baralho, size = 3, replace = FALSE)
    
    i <- 1
    azul <- 0
    
    while(i <= 3){
      
      if(mao[i] == "a"){
        
        azul <- azul + 1
        
      }
      
      i <- i + 1
      
    }
    
    if(azul == 2){
      
      vezes[k] <- 1
      
    }else{
      
      vezes[k] <- 0 
      
    }
    
  }
  
  }
  
  return(mean(vezes))
  
}

sorteio_azul(100000)


#1 - b)



#2 - a)

clientes <- read.table(file = "churn.txt", head = TRUE, sep = ";")

str(clientes)
summary(clientes)

clientes <- clientes[, -1]
clientes <- na.omit(clientes)

clientes$EstimatedSalary <- as.integer(clientes$EstimatedSalary)
clientes$Balance <- as.integer(clientes$Balance)

#2 - b)

n <- round(0.75 * nrow(clientes))

indices <- sample(1:nrow(clientes), size = n, replace = FALSE)

treino <- clientes[indices, ]
teste <- clientes[-indices, ]

library(rpart)
library(rpart.plot)
library(caret)

arvore <- rpart(formula = clientes$Exited~., data = treino, method = "class")

rpart.plot(arvore, extra = 101)

previsao <- predict(arvore, newdata = teste, type = "class")

mean(previsao == teste$Exited)

confusionMatrix(table(arvore, teste))

#2 - c)




