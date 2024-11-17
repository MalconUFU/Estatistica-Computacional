#Malcon Rezende Rodrigues - 12211BCC034

#1

#a)

library(ggplot2)

peixe_boi <- read.table(file = "peixe_boi.txt", header = TRUE, sep = ";")

ggplot(data = peixe_boi, aes(x = peixe_boi$mortes, y = peixe_boi$barcos, col = peixe_boi$ano)) + geom_point() + labs(title = "Mortes de Peixes-boi e Lanchas registradas", x = "Mortes", y = "Lanchas Registradas", col = "Anos") + theme_minimal()

#No gráfico, podemos observar uma relação linear entre as váriveis, ou seja, ao longo dos anos, o número de mortes de peixes-boi causadas por lanchas aumenta proporcionalmente ao número de lanchas registradas.

#b)

cor(peixe_boi$mortes, peixe_boi$barcos)

#A correlação entre as mortes de peixes-boi e um número de lanchas registradas é estatísticamente alto, cerca de 95%, indicando uma forte relação linear entre as váriaveis.

#c)

modelo_linear <- lm(data = peixe_boi, formula = peixe_boi$mortes ~ peixe_boi$barcos)
modelo_linear

#mortes = 0.1322 * barcos - 44.7218

summary(modelo_linear)

#Por meio da função lm, obtemos os coeficientes da equação que relaciona o número de mortes e o número de lanchas registradas (#mortes = 0.1322 * barcos - 44.7218). Analisando o modelo linear que obtívemos por meio da função summary, obtemos um p-valor (< 2.2e-16) baixo, entre 0 e 0.001, indicando um relação estatisticamente significativa entre as variáveis. Entretanto, os erros/resíduos são altos (MAX 16.9811), indicando que o modelo por não ser tão acertivo.

ggplot(data = peixe_boi, aes(x = peixe_boi$mortes, y = peixe_boi$barcos, col = peixe_boi$ano)) + geom_point() + labs(title = "Mortes de Peixes-boi e Lanchas registradas", x = "Mortes", y = "Lanchas Registradas", col = "Anos") + theme_minimal() + geom_smooth(method = "lm")

#d)

#H0: os residuos seguem um distribuição normal
#Ha: os residuos não seguem um distribuição normal

hist(modelo_linear$residuals)

shapiro.test(modelo_linear$residuals)

#O teste de Shapiro retornou um p-valor de 0.94, que é superior ao nível de significância de 5%. Como o p-valor é maior que o limite crítico, não há motivos para rejeitar a hipótese nula H0, que afirma que os resíduos seguem uma distribuição normal. Isso indica que os resíduos do modelo são normalmente distribuídos.

#e)

#800.000 lanchas. Nosso modelo está padronizado em milhares, ou seja, colocaremos 800 na fórmula

mortes_Fl = 0.1322 * 800 - 44.7218
mortes_Fl

#Após a limitação da Flórida para 800.000 lanchas registradas, que ainda é um número consideravelmente alto, podemos esperar, com base no nosso modelo, que cerca de 61 peixes-boi morrerão.

#f)

#Cálculo errado

mortes_Fl = 0.1322 * 200 - 44.7218
mortes_Fl

#Nosso modelo é limitado, sua análise é feita com base nos dados que foram previamente passados. Para dados que estão acima ou abaixo da amostra (abaixo de 400 e acima de 1010), o modelo pode errar bastante, por exemplo, no cálculo acima o número de mortes é negativo, o que estatísticamente não faz sentido.

#2)

musicas <- read.table(file = "musicas.txt", header = TRUE, sep = ";")

str(musicas)

musicas$artista <- as.factor(musicas$artista)

dados_padronizados <- scale(musicas[,-c(7,8)])

modelo_kmeans <- kmeans(dados_padronizados, centers = 3)
cluster <- modelo_kmeans$cluster

musicas$cluster_k3 <- as.factor(cluster)

ggplot(data = musicas, aes(x = musicas$cluster_k3, fill = musicas$artista)) + geom_bar() + labs(title = "Distribuição dos Clusters por Artista", x = "Cluster k3", fill = "Artistas") + theme_minimal()

#No gráfico, observamos o agrupamentos em clusteres de características (danceability, energy, loudness, speechiness, acousticness, instrumentalness) presentes nas músicas de três artistas, com o preenchimento das barras indicando o "domínio" do artista naquele cluster. 
#Cluster 1: A presença do Pato Fu é predominante, não havendo presença nem do Cartola e nem dos Racionais. Isso significa que as músicas do Pato Fu apresentam características que as dos outros artistas não possuem, de forma que ele domina um cluester próprio. 
#Cluster 2: Esse cluster é exclusivamente composto pelos Racionais MC's, mais uma vez indicando características únicas nas suas músicas. 
#Cluster 3: Nesse cluster há presença de todos os artistas, indicando características mais gerais que são presentes nas músicas dos três.
