#OPERACOES COM VETORES

a <- c(1, 2, 5)

b <- c(10, 12)

d <- c(a, 6)

c <- a + b

3 * a

d + b

#ESTRUTURA DE REPETICAO EM R

x <- 0
for(i in 1:20){
  
  x <- x + i
  
}
print(x)

aniversario <- sample(x = 1:365, size = 10, replace = TRUE)

aniversario

duplicated(aniversario) #any(duplicated(aniversario))

resultados <- c()

for(j in 1:10000){
  
  aniversario <- sample(x = 1:365, size = 23, replace = TRUE)
  resultados[j] <- any(duplicated(aniversario))
}

mean(resultados)

calcula_probabilidade <- function(n){
  
  resultados <- c()
  
  for(j in 1:10000){
    
    aniversario <- sample(x = 1:365, size = n, replace = TRUE)
    resultados[j] <- any(duplicated(aniversario))
  }
  
  return(mean(resultados))
}

calcula_probabilidade(n = 23)

#MEGASENA

bilhete <- c(1, 2, 3, 4, 5, 6)

sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
sorteio

bilhete %in% sorteio

semanas <- 0
acertos <- 0
  
while(acertos < 4){
    
    sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
    acertos <- sum(bilhete %in% sorteio)
    semanas <- semanas + 1
    
  }

semanas/52

#FUNCAO MEGASENA

calcula_bilhete <- function(a, b, c, d, e, f){
  
  semanas <- 0
  acertos <- 0
  bilhete <- c(a, b, c, d, e, f)
    
  while(acertos < 4){
    
    sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
    acertos <- sum(bilhete %in% sorteio)
    semanas <- semanas + 1
    
    }
  
  }
 
  return(semanas/52) 


calcula_bilhete(2, 3, 41, 50, 23, 12)

#MEDIA EM ANOS PARA GANHAR NA MEGASENA

calcula_bilhete_media <- function(a, b, c, d, e, f){

  bilhete <- c(a, b, c, d, e, f)
  anos <- 0

  for(k in 1:1000){
  
    semanas <- 0
    acertos <- 0
  
    while(acertos < 4){
  
      sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
      acertos <- sum(bilhete %in% sorteio)
      semanas <- semanas + 1
  
    }
  
    anos <- anos + semanas/52
  
  }

  return(anos/1000)

}

calcula_bilhete_media(1, 2, 3, 4, 5, 6)



