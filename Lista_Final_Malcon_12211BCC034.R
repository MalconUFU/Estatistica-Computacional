#Malcon Rezende Rodrigues - 12211BCC034

#1

diabetes <- read.table(file = "diabetes.txt", header = TRUE, sep = ";")

diabetes <- na.omit(diabetes)

diabetes <- diabetes[,-c(1,2)]
diabetes$Diabetic <- as.factor(diabetes$Diabetic)

n <- round(0.8 * nrow(diabetes))

indices <- sample(x = 1:nrow(diabetes), size = n, replace = FALSE)

treino <- diabetes[indices,]
teste <- diabetes[-indices,]

#a)

library(ggplot2)

ggplot(treino, aes(x = treino$PlasmaGlucose, fill = treino$Diabetic)) + geom_histogram() + scale_fill_manual(values = c("orange", "purple"), labels = c("Sem Diabetes", "Com Diabetes")) + labs(title = "Distribuição da Glicose", x = "Concentração de Glicose", y = "Indivíduos", fill = "Diabetes") + theme_minimal()

#O histograma compara a distribuição dos níveis de glicose no plasma entre pacientes com e sem diabetes. Pacientes com diabetes tendem a ter concentrações de glicose no plasma mais altas (pico acima de 120) em comparação com aqueles sem a doença. Isso pode indicar que níveis elevados de glicose estão associados ao diagnóstico de diabetes.

ggplot(treino, aes(x = treino$Diabetic , y = treino$BMI)) + geom_boxplot() + labs(title = "Relação entre o IMC e Diabetes em Indivíduos", x = "Diabetes", y = "IMC") + scale_x_discrete(labels = c("0" = "Sem diabetes", "1" = "Com diabetes")) + theme_minimal()

#O boxplot compara as medianas e a dispersão do IMC entre os indíviduos com e sem diabetes. O grupo com diabetes apresenta valor médio de IMC mais alto (acima de 30), isso pode indicar uma relação entre maior IMC e risco de diabetes.

ggplot(treino, aes(x = Age, fill = Diabetic)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("lightblue", "salmon"), labels = c("Sem Diabetes", "Com Diabetes")) + labs(title = "Distribuição da Idade por Condição de Diabetes", x = "Idade", fill = "Diabetes") + theme_minimal()

#O gráfico de densidade mostra a distribuição das idades entre os dois grupos. Indivíduos com diabetes tendem a ser mais velhos do que aqueles sem a doença, o que indica que a idade pode estar associada a um maior risco de diabetes.

##################################################################

#b)

library(rpart)
library(rpart.plot)

arvore <- rpart(Diabetic ~ ., data = treino, method = "class",
                control = rpart.control(cp = 0.02, maxdepth = 3))

rpart.plot(arvore, extra = 101)

algoritmo_diabetes <- function(idade, imc){
  
    if (idade < 36) {
      if (imc < 22) {
        return("Sem diabetes")
      } else {
        if (imc >= 36) {
          return("Sem diabetes")
        } else {
          return("Com diabetes")
        }
      }
    } else {
      if (imc < 22) {
        return("Sem diabetes")
      } else {
        if (idade >= 66) {
          return("Sem diabetes")
        } else {
          return("Com diabetes")
        }
      }
    }
  
}

algoritmo_diabetes(23, 41.51152)

previsao <- predict(arvore, newdata = teste, type = "class")

acuracia <- mean(previsao == teste$Diabetic)
acuracia

##################################################################

#c)

library(randomForest) 

floresta <- randomForest( formula = Diabetic ~ ., data = treino, ntree = 200)
floresta

previsao_floresta <- predict(floresta, newdata = teste, type = "class")

acuracia_floresta <- mean(previsao_floresta == teste$Diabetic)
acuracia_floresta

##################################################################

#d)

#O modelo de árvore de decisão apresentou uma acurácia de 81,3%, o que significa que ele classifica corretamente os casos de diabetes em aproximadamente 81% das vezes. Em comparação, o modelo de floresta aleatória obteve uma acurácia de 91,2%, identificando corretamente os casos de diabetes em 91% das vezes. Isso implica que a taxa de erro da árvore de decisão é de cerca de 19%, enquanto o erro da floresta aleatória é reduzido para 9,1%. Esses resultados indicam que a floresta aleatória é mais eficaz para o diagnóstico de diabetes, tornando-se a melhor escolha entre os dois modelos. Entretanto, está longe de ser um modelo ideal.

##################################################################

#e)

#O modelo de árvore de decisão alcançou uma acurácia de 81,3%, o que significa que ele classificou corretamente cerca de 81% dos casos totais. No entanto, ele teve dificuldades em identificar com precisão pacientes que realmente tinham diabetes, apresentando uma porcentagem de erro relativamente alta. Por outro lado, o modelo de floresta aleatória teve um desempenho melhor, com uma acurácia de 91,2%, o que indica que ele foi mais eficaz na identificação correta de pacientes com diabetes e reduziu significativamente o número de erros do tipo falso negativo. Portanto, a floresta aleatória se mostrou mais precisa e confiável para o diagnóstico de diabetes, tornando-se a melhor escolha entre os dois modelos.

##################################################################

#2

#a)

cerebelo <- read.csv(file = "cerebelo.csv", header = TRUE, sep = ",")

ggplot(data = cerebelo, aes(x = cerebelo$Cerebellum_g, y = cerebelo$Body_g, col = cerebelo$Species)) + geom_point() + theme_minimal()

ggplot(data = cerebelo, aes(x = cerebelo$Log_cerebellum, y = cerebelo$Log_body, col = cerebelo$Species)) + geom_point() + theme_minimal()

#No primeiro gráfico, que utiliza os valores absolutos em gramas, não se observa uma relação clara entre o peso corporal e o peso do cerebelo dos animais. Isso ocorre porque os valores absolutos de peso variam bastante entre as espécies, o que gera uma dispersão de dados e dificulta a identificação de um padrão. No entanto, ao transformar os valores para a escala logarítmica, a relação entre o peso do corpo e o peso do cerebelo se torna mais visível. Com a transformação logarítmica, percebemos uma tendência de crescimento que sugere uma relação proporcional entre o peso corporal e o peso do cerebelo, ainda que essa relação não seja estritamente linear.

##################################################################

#b)

cor(cerebelo$Body_g, cerebelo$Cerebellum_g)

##################################################################

#c)

  cor(cerebelo$Log_body, cerebelo$Log_cerebellum)

##################################################################

#d)

#A correlação entre o peso absoluto do corpo e o peso do cerebelo é relativamente baixa, em torno de 34%. Esse valor indica uma relação fraca entre essas variáveis quando expressas em suas unidades originais (gramas), sugerindo que o aumento no peso do corpo não influencia um aumento proporcional do peso do cerebelo. Essa falta de correlação clara ocorre devido à grande variação de tamanhos corporais entre as espécies. Por outro lado, ao transformar os valores em logaritmos, o coeficiente de correlação sobe para 95%, o que indica uma forte relação linear entre o logaritmo do peso corporal e o logaritmo do peso do cerebelo. Essa alta correlação sugere que, em uma escala logarítmica, existe uma relação proporcional entre o peso do corpo e o peso do cerebelo.

##################################################################

#e)

modelo_linear <- lm(data = cerebelo, cerebelo$Log_cerebellum ~ cerebelo$Log_body)
modelo_linear

#cerebelo = (0.7828 * peso) - 2.1574 Equação da Reta

summary(modelo_linear)

#Tanto o intercepto quanto a inclinação apresentam valores de p entre 0 e 0.001, indicando uma relação estatisticamente significativa entre o log do peso corporal e o log do peso do cerebelo. Além disso, os resíduos, que representam as diferenças entre os valores observados e os previstos pelo modelo, estão próximos de zero. Esse resultado indica que o modelo captura a relação entre as variáveis.

ggplot(data = cerebelo, aes(y = cerebelo$Log_body, x = cerebelo$Log_cerebellum)) + geom_point() + theme_minimal() + geom_smooth(method = "lm")

##################################################################

#f)

#H0: os residuos seguem um distribuição normal
#Ha: os residuos não seguem um distribuição normal

shapiro.test(modelo_linear$residuals)

hist(modelo_linear$residuals)

#O teste de Shapiro retornou um p-valor de 1, que é significativamente superior ao nível de significância de 5%. Como o p-valor é muito maior que o limite crítico, não há motivos para rejeitar a hipótese nula H0, que afirma que os resíduos seguem uma distribuição normal. Isso indica que os resíduos do modelo são normalmente distribuídos.

##################################################################

#g)

#log10 100000 = 5

cerebelo_100000 <- 5 * 0.7828 - 2.1574
cerebelo_100000 <- 10**cerebelo_100000
cerebelo_100000

##################################################################

#3

#a)

oliva <- read.table(file = "olive.txt", header = TRUE, sep = ",")

str(oliva)

dados_padronizados <- scale(oliva[,-1])

##################################################################

#b)

library(ggplot2)

modelo_kmeans <- kmeans(dados_padronizados, centers = 3)

cluster <- modelo_kmeans$cluster

oliva$cluster_k3 <- as.factor(cluster)

ggplot(data = oliva, aes(x = oliva$cluster_k3, fill = oliva$region)) + geom_bar() + labs(title = "Distribuição dos Clusters por Região", x = "Cluster k3", y = "Número de Azeites", fill = "Região") + theme_minimal()

#No gráfico, observamos a distribuição dos azeites italianos agrupados em três clusters, cada um representando composições específicas de ácidos graxos, com o preenchimento das barras indicando a origem regional dos azeites. Cluster 1: Esse cluster possui mais de 200 azeites e inclui azeites das três regiões da Itália. No entanto, há uma predominância de azeites de Southern Italy e uma quantidade significativa, mas menor, de azeites de Northern Italy e Sardinia. Isso sugere que o perfil de ácidos graxos do cluster 1 é mais diversificado, sendo comum a todas as regiões, mas com uma maior representação do sul. Cluster 2: Nesse cluster, vemos uma predominância de azeites de Northern Italy, com uma pequena presença de azeites de Southern Italy e nenhuma representação de Sardinia. A ausência de azeites de Sardinia e a presença majoritária de azeites do norte indicam que a composição de ácidos graxos desse cluster é bastante característica do norte da Itália. Cluster 3: Esse cluster é exclusivamente composto por azeites de Southern Italy. A predominância absoluta dessa região sugere que o perfil de ácidos graxos dos azeites do cluster 2 é altamente característico do sul da Itália.

##################################################################

#c)

#k = 4
modelo_kmeans_4 <- kmeans(dados_padronizados, centers = 4)
cluster_4 <- modelo_kmeans_4$cluster
oliva$cluster_k4 <- as.factor(cluster_4)

ggplot(data = oliva, aes(x = cluster_k4, fill = region)) + geom_bar() + labs(title = "Distribuição dos Clusters por Região", x = "Cluster k4", y = "Número de Azeites", fill = "Região") + theme_minimal()

#No gráfico, os azeites estão agrupados em quatro clusters. Cluster 1: observamos que o Cluster 1 é composto exclusivamente por azeites de Northern Italy, o que indica um perfil de ácidos graxos típico dessa região. Cluster 2: há uma grande quantidade de azeites de Northern Italy, uma representação significativa de azeites da Sardinia e uma pequena porcentagem de azeites de Southern Italy, sugerindo uma composição mista de ácidos graxos que é comum a essas três regiões. Cluster 3: é composto principalmente por Southern Italy, com uma pequena presença de Northern Italy. Cluster 4: é exclusivamente composto por azeites de Southern Italy, evidenciando que esse perfil de ácidos graxos é altamente característico dessa região.

#k = 5
modelo_kmeans_5 <- kmeans(dados_padronizados, centers = 5)
cluster_5 <- modelo_kmeans_5$cluster
oliva$cluster_k5 <- as.factor(cluster_5)

ggplot(data = oliva, aes(x = cluster_k5, fill = region)) + geom_bar() + labs(title = "Distribuição dos Clusters por Região", x = "Cluster k5", y = "Número de Azeites", fill = "Região") + theme_minimal()

#o gráfico, os azeites estão agrupados em cinco clusters. Cluster 1: é composto predominantemente por azeites de Northern Italy, com uma pequena presença de Southern Italy, sugerindo uma composição específica para o norte. Cluster 2: é composto exclusivamente por azeites de Southern Italy, indicando uma composição de ácidos graxos única para essa região. Cluster 3: presença exclusiva de azeites de Northern Italy, indicando um perfil característico dessa região. Os Clusters 4 e 5 mostram representações quase iguais entre azeites de Southern Italy e Sardinia, com uma pequena porcentagem de azeites provenientes de Northern Italy. Essa composição indica que os azeites do Northern Italy compartilham características de ácidos graxos com ambas as regiões, Sardinia e Southern Italy, enquanto estas duas últimas não compartilham características entre si.