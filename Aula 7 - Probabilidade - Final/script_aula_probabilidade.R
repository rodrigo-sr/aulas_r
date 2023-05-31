## ---------------
## Curso de Introducao ao R
## 
## ---------------

## ---
## Distribuicao Binomial
## ---

## bora gerar dados

## n: tamanho amostral a ser gerado
## size: numero de tentativas
## prob: probabilidade de sucesso

## Suponha que um aluno esteja fazendo uma prova de 20 questoes independentes entre si
## e a probabilidade do estudante acertar cada questão é de 0.2 (A,B,C,D,E). Gere dados
## para toda a turma.

dados = rbinom(n = 30, size = 20, prob = 0.2)
dados

hist(dados,
     main = "Histograma dos dados da binomial",
     xlab = "Frequência", 
     ylab = "Amostra")

## media

teoria_media = 20*0.2
teoria_media

mean(dados)

# variancia 

teoria_var =  20*0.6*(1-0.6)
teoria_var

var(dados)


## caso de uma bernouli
## somente sucesso e fracasso
## apenas uuma questao, em que eu estaria sujeito a responder 30 vezes

dados.bernouli = rbinom(n = 30, size = 1, prob = 0.2)
dados.bernouli

hist(dados.bernouli,
     main = "Histograma dos dados da bernoulli",
     xlab = "Frequência", 
     ylab = "Amostra")

## calulo para um exemplo aplicado

## Uma moeda honesta e lançada 100 vezes. Qual a probabilidade de cair duas caras?
## Como a moeda e honesta, a probabilidade de cair cara em cada ensaio é igual a 0,5 ou 50%.
## Qual a probabilidade de, dentre as tentativas, essa moeda ser honesta (metade cara e
## metade coroa):

tentativas = 100
probabilidade = 0.5

pbinom(q = tentativas/2, size = tentativas, prob = probabilidade)


## Agora vamos descobrir a probabilidade exclusiva de cada ponto?

## voltando para o exemplo da prova aplicada na turma

## entre 10 tentativas, eu quero estudar o sucesso em ter 1, 2, 3, até 10 coroas.

tamanho = 20
tipos_sucesso = 1:tamanho

prob_tipos_sucesso = dbinom(x = tipos_sucesso, size = tamanho, prob = 0.2)

cbind(tipos_sucesso, prob_tipos_sucesso)

barplot(height = prob_tipos_sucesso,
        names = tipos_sucesso,
        xlab = "Valores da variavel aleatoria",
        ylab = "Probabilidade",
        col = "steelblue")

## ---
## Poisson
## ---

## Suponha que um determinado em uma fábrica de laticinios,
## na etapa de embalagens podem ocorrer falhas, de acordo com
## uma variável aleatória X que segue uma distribuição de Poisson
## de parâmetro λ = 3.

parametro = 3

## Estamos interessados na probabilidade de 5 caixas de
## leite apresentarem alguma deformação.

dpois(x = 5, lambda = 5)

## Bora generalizar para de 0 (zero) ate 20?

variavel_aleatoria = 1:20
probabilidade_poisson = dpois(x = variavel_aleatoria, lambda = parametro)

cbind(variavel_aleatoria, probabilidade_poisson)

barplot(height = probabilidade_poisson,
        names = variavel_aleatoria,
        xlab = "Valores da variavel aleatoria",
        ylab = "Probabilidade",
        col = "steelblue")


## Como estudar a probabilidade de pelo menos 
## observar acima de 5 falhas na producao de emabalagens

ate_3_falhas = ppois(3, lambda = 3)

1 - ate_3_falhas


## geracao de dados

## Em um determinado hospital neonatal observa-se que a ocorrencia 
## de abortos espotaneos ocorre, em media, 2 vezes a cada mes

## gere 24 valores para representar a ocorrencia desse problema
## em dois anos, isto e, 24 messes.

amostra_poisson = rpois(n = 20, lambda = 2)

## vamos ver se a media, bate com a formulacao

mean(amostra_poisson)

## e a variancia

var(amostra_poisson)

## vamos observar como se comporta essas probabilidades

hist(amostra_poisson, freq = F,
     ylab = "Densidade")

## ---
## Distribuicao normal
## ---

# Um exemplo bem aplicado da distribuição normal na economia é o modelo
# CAPM (Capital Asset Pricing Model), amplamente utilizado para estimar 
# o retorno esperado de um ativo financeiro.

# Suponha que você esteja analisando uma ação específica e queira estimar 
# seu retorno esperado e risco. Você coleta dados históricos dos retornos 
# dessa ação e verifica que eles seguem aproximadamente uma distribuição
# normal, com uma média de 8% e um desvio padrão de 12%.

amostra_normal = rnorm(n = 1000, mean = 8, sd = 12)

hist(amostra_normal,
     xlab = "Valores da variavel aleatoria",
     ylab = "Probabilidade",
     col = "steelblue")

mean(amostra_normal)
sd(amostra_normal)

## Qual a probabilidade do retorno do ativo financeiro seja menor que a média (8)

pnorm(q = 8, mean = 8, sd = 12)

## e acima da media?

1 - pnorm(q = 8, mean = 8, sd = 12)

## qual a probabilidade de estar entre 5 e 10?

pnorm(q = 10, mean = 8, sd = 12) - pnorm(q = 5, mean = 8, sd = 12) 

## Qual a probabilidade de ser acima de 10?

1 - pnorm(q = 10, mean = 8, sd = 12)























