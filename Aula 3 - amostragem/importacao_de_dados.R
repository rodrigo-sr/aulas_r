#instalação de pacotes
# install.packages("<nome_do_pacote_com_aspas>") # Instalação de pacotes

# library(<nome_do_pacote_sem_aspas>) # Carregamento de pacotes

# Funções básicas
# mean()     # calcula a média
# sort()     # ordena os dados
# head()     # mostra as 6 primeiras linhas
# tail()     # mostra as 6 últimas linhas
# unique()   # mostra os valores únicos
# subset()   # filtra os dados
# colnames() # mostra os nomes das colunas
# table()    # mostra a frequência dos valores
# c()        # estrutura de dados: vetor
# plot()     # cria um gráfico
# barplot()  # cria um gráfico de barras
# split()    # divide os dados em estratos
# sample()   # amostragem aleatória simples
# round()    # arredonda os valores
# nrow()     # retorna o número de linhas

#filtrar dados
# <nome_da_variável_que_está_armazenando_os_dados>[<indice_inicial>:<indice_final>] #fatiamento por índice
# subset(<variavel_que_contem_os_dados>, <expressao_logica>) #filtragem por expressão lógica
# subset(<variavel_que_contem_os_dados>, <expressao_logica> & <expressão_lógica>) #filtragem por expressão lógica
# subset(<variavel_que_contem_os_dados>, <expressao_logica> | <expressão_lógica>) #filtragem por expressão lógica


#importação de dados
dados_empresas <- read.csv('https://raw.githubusercontent.com/rs9789/aulas_r/main/dados/empresas_unicornio.csv', sep = ',')
head(dados_empresas)
colnames(dados_empresas)

#modificando o nome das colunas
novo_nome_colunas <- c('Empresa', 'Valor_em_B', 'Data inicio', 'Pais', 'Cidade', 'Industria', 'Investidores')
colnames(dados_empresas) <- novo_nome_colunas

colnames(dados_empresas)

#########################
#tratamento valor em B
dados_empresas$Valor_em_B <- as.numeric(gsub("\\$", "", dados_empresas$Valor_em_B))

#########################

#verificando valores únicos na coluna Pais
unique(dados_empresas$Pais)

#criando um gráfico de barras com a frequência de empresas por país
numero_empresas_por_pais <- table(dados_empresas$Pais)
numero_empresas_por_pais



numero_empresas_pais_ordenado <- sort(numero_empresas_por_pais, decreasing = TRUE)
numero_empresas_pais_ordenado

#vetor de paises com mais empresas
filtro_paises <- c("United States", "China", "India", "United Kingdom",
"Germany", "France", "Israel", "Canada", "Brazil", "South Korea")


#armazenando os 10 primeiros países com mais empresas
top10_paises_com_mais_empresas <- subset(dados_empresas, Pais %in% filtro_paises)
top10_paises_com_mais_empresas


# gráfico de barras com os 10 primeiros países com mais empresas
barplot(sort(table(top10_paises_com_mais_empresas$Pais), decreasing = FALSE),
             horiz = TRUE, las=TRUE)

#media do valor em bilhões de empresas do Estados Unidos
media_estados_unidos1 <- mean(top10_paises_com_mais_empresas[top10_paises_com_mais_empresas$Pais == 'United States', 'Valor_em_B'])
media_estados_unidos1

#média do valor em bilhões de empresas chinesas
media_china1 <- mean(top10_paises_com_mais_empresas[top10_paises_com_mais_empresas$Pais == 'China', 'Valor_em_B'])
media_china1

#definindo o n
n <- 100

# Realizando amostragem aleatória simples 
amostra_aleatoria_simples <- top10_paises_com_mais_empresas[sample(nrow(top10_paises_com_mais_empresas), n),]
amostra_aleatoria_simples

media_estados_unidos_aas <- mean(amostra_aleatoria_simples[amostra_aleatoria_simples$Pais == 'United States', 'Valor_em_B'])
media_estados_unidos_aas

media_china_aas <- mean(amostra_aleatoria_simples[amostra_aleatoria_simples$Pais == 'China', 'Valor_em_B'])
media_china_aas


# Realizando amostragem estratificada
# Com o n definido temos:

# n para cada país
n1 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'United States')) / nrow(top10_paises_com_mais_empresas))
n2 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'China')) / nrow(top10_paises_com_mais_empresas))
n3 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'India')) / nrow(top10_paises_com_mais_empresas))
n4 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'United Kingdom')) / nrow(top10_paises_com_mais_empresas))
n5 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'Germany')) / nrow(top10_paises_com_mais_empresas))
n6 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'France')) / nrow(top10_paises_com_mais_empresas))
n7 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'Israel')) / nrow(top10_paises_com_mais_empresas))
n8 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'Canada')) / nrow(top10_paises_com_mais_empresas))
n9 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'Brazil')) / nrow(top10_paises_com_mais_empresas))
n10 <- round(n * nrow(subset(top10_paises_com_mais_empresas, Pais == 'South Korea')) / nrow(top10_paises_com_mais_empresas))


# install.packages('sampling')
library(sampling)

amostra_estratificada <- strata(top10_paises_com_mais_empresas, c('Pais'), c(n2,n1,n4, n3, n5, n8, n6, n7, n9, n10), method="srswor")
top10_paises_com_mais_empresas[unlist(amostra_estratificada['ID_unit']),]
amostra_estratificada

#plotando o gráfico para comparar as amostras
barplot(sort(table(amostra_estratificada$Pais), decreasing = FALSE),
             main = 'Amostra Estratificada',
             xlab = 'Quantidade de empresas',
             ylab = 'País',
             horiz = TRUE)

barplot(sort(table(amostra_aleatoria_simples$Pais), decreasing = FALSE),
             main = 'Amostra Aleatória Simples',
             xlab = 'Quantidade de empresas',
             ylab = 'País',
             horiz = TRUE)
