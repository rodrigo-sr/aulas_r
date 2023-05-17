## -------
## Aula curso em R
## Estatistica Descritiva em gráficos
## -------

library(readxl)
library(ggplot2)
library(cowplot)

install.packages("DescriptiveStats.OBeu") ## distribuicao lasses Sturges
library(DescriptiveStats.OBeu)


setwd("C:\\Users\\NetoDavi\\Desktop\\aulas_r\\dados")

hatco = read_excel("hatco.xlsx")

head(hatco)

hatco = hatco[,-1]
colnames(hatco) = c("tempo_entrega", "nivel_preco", 
                    "flexibilidade_preco", "imagem_fabricante",
                    "servico_geral", "imagem_forca_vendas",
                    "qualidade_do_produto", 
                    "nivel_de_uso", "nivel_de_satisfacao", "tamanho_empresa",
                    "especificacoes_compras", "estrutura_aquisicao",
                    "tipo_industria", "tipo_situacao_compra")

## -------
## Sumario dos dados
## -------

summary(hatco)


## -------
## Valores unicos
## -------

unique(hatco$tamanho_empresa) ## unico
unique(hatco$estrutura_aquisicao)

## -------
## Distribuicao de Frequencia
## -------

## absoluta
table(hatco$tipo_situacao_compra)

## relativa
prop.table(table(hatco$tipo_situacao_compra))
round(prop.table(table(hatco$tipo_situacao_compra)), 2)


## organizando em uma tabelinha

dist_frequencia = cbind(table(hatco$tipo_situacao_compra), 
                  round(prop.table(table(hatco$tipo_situacao_compra)), 4)*100)

dist_freq = data.frame(frequencia = dist_frequencia[,1],
                       porcentagem = dist_frequencia[,2])


dist_freq

## -------
## Distribuicao de Frequencia
## Caso para variaveis quantitativas continuas
## -------

## formulas de Sturges

ds.hist(hatco$servico_geral, breaks = "Sturges", tojson = FALSE)



## configurando tamanho de letras 
tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

## ---
## Histograma
## ---

hist1 <- ggplot(data = hatco, mapping = aes(x = tempo_entrega)) +
  geom_histogram(fill = "cyan", linewidth = 1.2, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequência") +
  xlab("Tempo de entrega") +
  tema

hist1

## avaliando o tempo de entrega menor do que 2 semanas

df <- data.frame(price = dados["price"][dados["price"] <= 1000])

condicao_2semanas = hatco$tempo_entrega<=2
menor_2semanas = data.frame(tempo = hatco$tempo_entrega[condicao_2semanas ])

hist2 <- ggplot(data = menor_2semanas, mapping = aes(x = tempo)) +
  geom_histogram(fill = "cyan", bins = 7, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequência") +
  xlab("Tempo de entrega") +
  tema


hist2

## Juntando os dois histogramas em uma imagem so
plot_grid(hist1, hist2, ncol=2, nrow=1)

## ----
## histogramas estratificados
## ----


tema2 <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 21),
  axis.title.y = element_text(size = 21),
  legend.position = "none",
  strip.text.x = element_text(margin = margin(10, 10, 10, 10), size = 20))

options(repr.plot.width=15, repr.plot.height=12)

ggplot(data = hatco, mapping = aes(x = imagem_forca_vendas)) +
  geom_histogram(mapping = aes(fill = tipo_industria), bins = 7, size = 1, color = "black") +
  ylab("Frequência") +
  xlab("Imagem da força de vendas") +
  facet_wrap(~tipo_industria, nrow=1, ncol = 2) +
  theme_bw() +
  tema2 


## ---
## Grafico de Barras
## ---

## necessita do data.frame com a distribuição de frequencia

barplot.freq = ggplot(data = dist_freq, mapping = aes(x = frequencia, y = row.names(dist_freq))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(dist_freq),
                                            color = row.names(dist_freq)), alpha = .6, size = 1.1) +
  geom_label(mapping = aes(label=frequencia), fill = "#006400", size = 6, 
             color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  xlab("Frequência") +
  theme_bw() +
  tema

barplot.freq

barplot.pct = ggplot(data = dist_freq, mapping = aes(x = porcentagem, y = row.names(dist_freq))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(dist_freq),
                                            color = row.names(dist_freq)), alpha = .6, size = 1.1) +
  geom_label(mapping = aes(label=porcentagem), fill = "#006400", size = 6, 
             color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  xlab("Frequência") +
  theme_bw() +
  tema

barplot.pct


## position dodge

ggplot(hatco, aes(fill=tipo_industria, y=nivel_preco, x=tipo_situacao_compra)) + 
  geom_bar( stat="identity")



## ---
## Grafico por coordenadas polares
## Todos contra o grafico de pizza
## ---



b <- ggplot(data = dist_freq, mapping = aes(x = row.names(dist_freq), y = frequencia)) +
  geom_bar(stat = "identity", aes(fill = row.names(dist_freq)), color = "black", size = 1, alpha = .5) +
  xlab("") +
  ylab("") +
  tema 

b + coord_polar()


## ---
## Grafico de colunas
## ---




## ---
## Grafico de Densidade estratificado
## ---

tema <- theme(plot.background = element_rect(fill = "white"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 21),
              axis.title.y = element_text(size = 21),
              legend.position = "none")

na <- c("Tribeca", "Sea Gate", "Riverdale", "Prince's Bay", "Battery Park City", "Flatiron District", "Randall Manor", "NoHo", "SoHo", "Midtown")
df <- data.frame(neighbourhood = dados$neighbourhood[dados$neighbourhood == na], price = dados$price[dados$neighbourhood == na])

ggplot(data = df, mapping = aes(x = price, y = neighbourhood)) +
  geom_density_ridges(mapping = aes(fill = neighbourhood), bandwidth = 130, alpha = .6, size = 1.5) +
  theme_economist() +
  xlab("Price") +
  ylab("") +
  ggtitle("Price behavior in relation to neighborhoods") +
  tema

## -----
## grafico de violino
## ---


#  Violin Plot
base4 + geom_violin(aes(x = status), fill = "green")



## -----
## grafico de dispersao
## ---

ggplot(by_year, aes(year, percent_yes)) +
  geom_point() +
  geom_smooth()


filtered_by_country_year_topic <- by_country_year_topic %>%
  filter(country %in% c("United States", "United Kingdom", "France", "Germany"))

# Plot of percentage "yes" over time, faceted by topic
ggplot(filtered_by_country_year_topic, aes(x = year, y = percent_yes, color = country)) +
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~ topic)


