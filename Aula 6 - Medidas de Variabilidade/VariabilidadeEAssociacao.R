dados <- iris

View(dados)

boxplot(Sepal.Length ~ Species, data = dados)
boxplot(Sepal.Width ~ Species, data = dados)
boxplot(Petal.Length ~ Species, data = dados)
boxplot(Petal.Width ~ Species, data = dados)


unique(dados$Species)

setosa <- subset(dados, Species == "setosa")
versicolor <- subset(dados, Species == "versicolor")
virginica <- subset(dados, Species == "virginica")

# Variância
# Olhando para todos os dados
var(dados$Sepal.Length)
var(dados$Sepal.Width)
var(dados$Petal.Length)
var(dados$Petal.Width)

# verificando a variância para cada grupo
var(setosa$Sepal.Length)
var(setosa$Sepal.Width)
var(setosa$Petal.Length)
var(setosa$Petal.Width)

# Desvio padrão
# Olhando para todos os dados
sd(dados$Sepal.Length)
sd(dados$Sepal.Width)
sd(dados$Petal.Length)
sd(dados$Petal.Width)

# verificando o desvio padrão para cada grupo
sd(setosa$Sepal.Length)
sd(setosa$Sepal.Width)
sd(setosa$Petal.Length)
sd(setosa$Petal.Width)

# Coeficiente de variação
# Olhando para todos os dados
# desvio padrão / média

sd(dados$Sepal.Length) / mean(dados$Sepal.Length)
sd(dados$Sepal.Width) / mean(dados$Sepal.Width)
sd(dados$Petal.Length) / mean(dados$Petal.Length)
sd(dados$Petal.Width) / mean(dados$Petal.Width)

# verificando o coeficiente de variação para cada grupo
sd(setosa$Sepal.Length) / mean(setosa$Sepal.Length)
sd(setosa$Sepal.Width) / mean(setosa$Sepal.Width)
sd(setosa$Petal.Length) / mean(setosa$Petal.Length)
sd(setosa$Petal.Width) / mean(setosa$Petal.Width)

#algumas visualizações

# histograma
hist(dados$Sepal.Length)
hist(dados$Sepal.Width)
hist(dados$Petal.Length)
hist(dados$Petal.Width)

# histograma para cada espécie
hist(setosa$Sepal.Length, xlab='Comprimento da Sépala', ylab='Frequência', main='Setosa', xlim=c(4, 6))
hist(setosa$Sepal.Width, xlab='Largura da Sépala', ylab='Frequência', main='Setosa', xlim=c(1.5, 5))


# gráfico de dispersão
plot(dados$Sepal.Length, dados$Sepal.Width, 
     xlab='Comprimento da Sépala', 
     ylab='Largura da Sépala', 
     main='Gráfico de Dispersão', col='blue', 
     pch=19, cex=3)

# gráfico de dispersão para cada espécie
plot(setosa$Sepal.Length, setosa$Sepal.Width,
     xlab='Comprimento da Sépala', ylab='Largura da Sépala',
     main='Gráfico de Dispersão', col='blue', 
     pch=19, cex=3)
