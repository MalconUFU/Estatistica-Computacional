iris

#PASSO 00: EMBARALHAR OS DADOS

iris <- iris[sample(nrow(iris)),]

#PASSO 01: DIVIDIR EM TREINO E TESTE
#ROUND: ARRENDONDAR O VALOR

n <- round(0.8 * nrow(iris)) #n = 120

treino <- iris[1:n,]
teste <- iris[-(1:n),]

#GRAFICOS

ggplot(data = treino, mapping = aes(x = Species)) + geom_bar() + theme_minimal()

ggplot(data = treino, mapping = aes(x = Petal.Length)) + geom_histogram(fill = "red", bins = 20, alpha = 0.7)

ggplot(data = treino, mapping = aes(y = Petal.Length)) + geom_boxplot() + facet_wrap(~Species)

ggplot(data = treino, mapping = aes(y = Sepal.Length)) + geom_boxplot() + facet_wrap(~Species)

ggplot(data = treino, mapping = aes(y = Petal.Width)) + geom_boxplot() + facet_wrap(~Species)

ggplot(data = treino, mapping = aes(y = Sepal.Width)) + geom_boxplot() + facet_wrap(~Species)

ggplot(data = treino, mapping = aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point(size = 2, alpha = 0.5) + theme_minimal()


resultados <- c()

for(j in 1:nrow(teste)){
  
  if(teste$Petal.Length[j] < 2.5){
    
      resultados[j] <- "setosa"
    
  }else{
    
      if(teste$Petal.Width[j] < 1.75){
        
        resultados[j] <- "versicolor"
        
      }else{
        
        resultados[j] <- "verginica"
        
    }
  }
}

mean(teste$Species == resultados)

