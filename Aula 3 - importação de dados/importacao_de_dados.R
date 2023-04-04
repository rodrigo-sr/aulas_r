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
novo_nome_colunas <- c('Empresa', 'Valor em B', 'Data inicio', 'Pais', 'Cidade', 'Industria', 'Investidores')
colnames(dados_empresas) <- novo_nome_colunas

colnames(dados_empresas)

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
