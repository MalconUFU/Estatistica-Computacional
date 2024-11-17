#modelo K Means

library(jpeg)
library(ggplot2)

gato <- readJPEG("gato.jpeg")

str(gato)

R <- as.vector(gato[,,1])
G <- as.vector(gato[,,2])
B <- as.vector(gato[,,3])

x <- rep(1:497, each = 349)
y <- rep(349:1, times = 497)

dados <- data.frame(x,y,R,G,B)
head(dados)

?kmeans

clusterizacao <- kmeans(x = dados[,3:5], centers = 3, nstart = 20)

clusterizacao$centers

cores <- rgb(clusterizacao$centers)

cluster <- as.factor(clusterizacao$cluster)

dados$cluster <- cluster
head(dados)

ggplot(data = dados, aes(x = x, y = y, col = cluster)) + geom_point() + scale_color_manual(values = cores) + theme_void()
