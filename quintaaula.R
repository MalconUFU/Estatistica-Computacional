install.packages("ggplot2")
library(ggplot2)

titanic <- read.table(file = "titanic.txt", sep = ",", header = TRUE)

titanic <- titanic[, -c(1, 9:12)]

titanic$Survived <- as.factor(titanic$Survived)

titanic$Pclass <- as.factor(titanic$Pclass)

titanic$Sex <- as.factor(titanic$Sex)

str(titanic)

#GRAFICOS

summary(titanic)

ggplot(data = titanic, aes(x = Survived)) + geom_bar(fill = "#f03b20") + theme_minimal()

ggplot(data = titanic, aes(x = Survived, fill = Sex)) + geom_bar() + scale_fill_manual(values = c("female" = "#f03b20", "male" = "#feb24c")) + theme_minimal()

ggplot(data = titanic, aes(x = Sex, fill = Survived))  + geom_bar() + labs(title = "Análise de sobrevivência de homens e mulheres por classe", x = "SEXO", y = "FREQUENCIA", fill = "SOBREVIVEU") + scale_fill_manual(values = c("0" = "#bcbddc", "1" = "#756bb1"), labels = c('0' = "NÃO", '1' = "SIM")) + facet_wrap(~Pclass) + scale_x_discrete(labels = c("female" = "mulher", "male" = "homem")) + theme_minimal()


