#IMPORTAR UM CONJUNTO DE DADOS PARA O R
dados <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
dados <- dados[, -1] #dados[,-c(1,12)]

#SOBRESCREVER COMO FATOR
dados$Survived <- as.factor(dados$Survived)

dados$Pclass <- as.factor(dados$Pclass)

dados$Sex <- as.factor(dados$Sex)

str(dados)

dados[c(6, 10),]

#RESUMO DOS DADOS
summary(dados)

#ANALISE DOS DADOS

homens <- dados[dados$Sex == "male", ]
#sum(homens$Survived == 1)

summary(homens)

table(homens$Survived)
barplot(table(homens$Survived))

mulheres <- dados[dados$Sex == "female",]
#sum(mulheres$Survived == 1)

summary(mulheres)

table(mulheres$Survived)
barplot(table(mulheres$Survived))

terceira_homens <- homens[homens$Pclass == 3,]
table(terceira_homens$Survived)

libray(ggplot2)

ggplot(data = homens, aes(x = Pclass, fill = Survived)) + geom_bar()
