
75*20/100

?iris


dados = iris

head(dados) # leitura dos dados


str(dados) # estrutura das variáveis do nosso banco

View(dados)


unique(dados$Species) # os 3 tipos de espécies 


range(dados$Sepal.Length) #o menor valor e o máximo do comprimento da sepala


range(dados$Petal.Length) #o o menor valor e o máximo do comprimento da pétala


##### medidas de tendencia central ####

#média

mean(dados$Sepal.Length)  # 5.84

#mediana

median(dados$Sepal.Length) #5.8


### primeiro e terceiro quartil 



quantile(dados$Sepal.Length)

### descritiva geral dos dados 

summary(dados)


## estratificando essa descritiva por grupo

by(dados, dados$Species, summary) #resumo de cada passo a passo anterior


###### gráficos ######

## instalação dos pacotes

if(!require("ggplot2")){install.packages("ggplot2")};library(ggplot2)


hist(dados$Sepal.Length)

ggplot(dados,aes(x=Sepal.Length))+
  geom_histogram(bins=17, fill=4, col="black")+
  labs(x="Comprimento da Sépala", y="Frequência")+
  theme_classic()




ggplot(dados,aes(x=Sepal.Width))+
  geom_histogram(bins=17, fill=4, col="black")+
  labs(x="Largura da Sépala", y="Frequência")+
  theme_classic()


#### carregando o pacote  ggpubr

#install.packages("ggpubr")

library(ggpubr)


ggboxplot(dados, x = "Species", y = "Sepal.Length",
          color = "Species",
          palette = c("blue", "red", "black"), xlab = "Espécies", 
          ylab = "Comprimento da Sepala")


df = dados

boxplot(df$Sepal.Length ~ df$Species, main = "Boxplot Sepal.Length por espécies", xlab = "Espécies", ylab = "Comprimento da Sépala")
