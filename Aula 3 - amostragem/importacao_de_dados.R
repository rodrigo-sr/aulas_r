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


dim(top10_paises_com_mais_empresas)

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

estratos <- split(top10_paises_com_mais_empresas, top10_paises_com_mais_empresas$Pais)

# retirando a amostra estratificada para cada país

amostra_estratificada1 <- estratos[['United States']][sample(1:nrow(estratos[['United States']]), n1),]
amostra_estratificada2 <- estratos[['China']][sample(1:nrow(estratos[['China']]), n2),]
amostra_estratificada3 <- estratos[['India']][sample(1:nrow(estratos[['India']]), n3),]
amostra_estratificada4 <- estratos[['United Kingdom']][sample(1:nrow(estratos[['United Kingdom']]), n4),]
amostra_estratificada5 <- estratos[['Germany']][sample(1:nrow(estratos[['Germany']]), n5),]
amostra_estratificada6 <- estratos[['France']][sample(1:nrow(estratos[['France']]), n6),]
amostra_estratificada7 <- estratos[['Israel']][sample(1:nrow(estratos[['Israel']]), n7),]
amostra_estratificada8 <- estratos[['Canada']][sample(1:nrow(estratos[['Canada']]), n8),]
amostra_estratificada9 <- estratos[['Brazil']][sample(1:nrow(estratos[['Brazil']]), n9),]
amostra_estratificada10 <- estratos[['South Korea']][sample(1:nrow(estratos[['South Korea']]), n10),]


media_estados_unidos_aae <- mean(amostra_estratificada1$Valor_em_B)
media_estados_unidos_aae

media_china_aae <- mean(amostra_estratificada2$Valor_em_B)
media_china_aae

dados_estratificados <- rbind(amostra_estratificada1, amostra_estratificada2, 
                                     amostra_estratificada3, amostra_estratificada4, 
                                     amostra_estratificada5, amostra_estratificada6, 
                                     amostra_estratificada7, amostra_estratificada8, 
                                     amostra_estratificada9, amostra_estratificada10)

barplot(sort(table(unindo_dados_estratificados$Pais), decreasing = FALSE),
             main = 'Amostra Estratificada',
             xlab = 'Quantidade de empresas',
             ylab = 'País',
             horiz = TRUE)

barplot(sort(table(amostra_aleatoria_simples$Pais), decreasing = FALSE),
             main = 'Amostra Aleatória Simples',
             xlab = 'Quantidade de empresas',
             ylab = 'País',
             horiz = TRUE)



#finaliza aqui


















#filtrando dados

#simples
empresas_brasileiras <- subset(dados_empresas, Pais == 'Brazil')
head(empresas_brasileiras)
empresas_brasileiras[1,]

unique(empresas_brasileiras$Industry)


# Múltipla condição 'e'
fintechs_brasileiras <- subset(dados_empresas, Pais == 'Brazil' & Pais == 'Fintech')
head(fintechs_brasileiras)
fintechs_brasileiras[1,]


# múltipla condição 'ou'

empresas_saopaulo_campinas <- subset(dados_empresas, Cidade == 'Sao Paulo' | Cidade == 'Campinas')
head(empresas_saopaulo_campinas)
empresas_saopaulo_campinas

# condição 'e' com 'ou'
empresas_brasileiras_fintechs_e_IA <- subset(dados_empresas, Pais == 'Brazil' & (Industria == 'Fintech' | Industria == 'Artificial intelligence'))
empresas_brasileiras_fintechs_e_IA


# visualizando informações

# tabela de frequência
numero_empresas_por_pais <- table(dados_empresas$Pais)
numero_empresas_por_pais

numero_empresas_por_industria <- table(dados_empresas$Industria)
numero_empresas_por_industria

numero_empresas_brasileiras_por_industria <- table(empresas_brasileiras$Industria)
numero_empresas_brasileiras_por_industria

# gráfico de barras

#parâmetros do gráfico de barras


barplot(numero_empresas_por_pais, horiz = TRUE)
barplot(numero_empresas_por_industria, horiz = TRUE)
barplot(numero_empresas_brasileiras_por_industria, horiz = TRUE)

# ajustando valores para melhorar a visualização
numero_empresas_por_pais_ordenado <- sort(numero_empresas_por_pais, decreasing = FALSE)
numero_empresas_por_industria_ordendado <- sort(numero_empresas_por_industria, decreasing = FALSE)
numero_empresas_brasileiras_por_industria_ordenado <- sort(numero_empresas_brasileiras_por_industria, decreasing = FALSE)

barplot(numero_empresas_por_pais_ordenado, horiz = TRUE)
barplot(numero_empresas_por_industria_ordendado, horiz = TRUE)
barplot(numero_empresas_brasileiras_por_industria_ordenado, horiz = TRUE)

#pegando as 10 primeiras posições para países e indústrias
top10_paises_com_mais_empresas <- tail(numero_empresas_por_pais_ordenado, 10)
top10_industrias_com_mais_empresas <- tail(numero_empresas_por_industria_ordendado, 10)

barplot(top10_paises_com_mais_empresas, horiz = TRUE)
barplot(top10_industrias_com_mais_empresas, horiz = TRUE)

# filanizando a visualização
barplot(top10_paises_com_mais_empresas, horiz = TRUE, las=TRUE)
barplot(top10_industrias_com_mais_empresas, horiz = TRUE, las=TRUE)
barplot(numero_empresas_brasileiras_por_industria_ordenado, horiz = TRUE, las=TRUE)