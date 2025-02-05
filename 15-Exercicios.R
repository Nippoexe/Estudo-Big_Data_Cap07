# Solução Lista de Exercícios

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
projeto_path = "D:/FCDados/[10] - Analise Estatistica 3/[03] - Projetos/"
input_path = "D:/FCDados/[10] - Analise Estatistica 3/[01] - InputData/"
output_path = "D:/FCDados/[10] - Analise Estatistica 3/[02] - OutputData/"
setwd(projeto_path)
getwd()


# Pacotes
#install.packages("dplyr")
#install.packages('nycflights13')
library('ggplot2')
library('dplyr')
library('nycflights13')
View(flights)
?flights

# Definindo o Problema de Negócio
# Crie um teste de hipótese para verificar se os voos da Delta Airlines (DL)
# atrasam mais do que os voos da UA (United Airlines)


##### ATENÇÃO #####
# Você vai precisar do conhecimento adquirido em outros capítulos do curso 
# estudados até aqui para resolver esta lista de exercícios!


# Exercício 1 - Construa o dataset pop_data com os dados de voos das 
# companhias aéreas UA (United Airlines) e DL (Delta Airlines). 
# O dataset deve conter apenas duas colunas, nome da companhia e atraso nos voos de chegada.
# Os dados devem ser extraídos do dataset flights para construir o dataset pop_data
# Vamos considerar este dataset como sendo nossa população de voos
?filter
?select
?slice_sample

pop_data <- filter(flights, carrier == 'DL' | carrier == 'UA',  arr_delay >= 0) %>% select(carrier, arr_delay)
pop_data

# Exercício 2  - Crie duas amostras de 1000 observações cada uma a partir do 
# dataset pop_data apenas com dados da companhia DL para amostra 1 e apenas dados 
# da companhia UA na amostra 2

# Dica: inclua uma coluna chamada sample_id preenchida com número 1 para a primeira 
# amostra e 2 para a segunda amostra
df_amostra1 <- filter(pop_data, carrier == 'DL') %>% slice_sample(n = 1000)
df_amostra2 <- filter(pop_data, carrier == 'UA') %>% slice_sample(n = 1000)
df_amostra1$sample_id = '1'
df_amostra2$sample_id = '2'

df_amostra1
df_amostra2

# Exercício 3 - Crie um dataset contendo os dados das 2 amostras criadas no item anterior. 
df_amostral <- rbind(df_amostra1, df_amostra2)
df_amostral
tail(df_amostral)
?rbind
# Exercício 4 - Calcule o intervalo de confiança (95%) da amostra1

# Usamos a fórmula: erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))

# Esta fórmula é usada para calcular o desvio padrão de uma distribuição da média amostral
# (de um grande número de amostras de uma população). Em outras palavras, só é aplicável 
# quando você está procurando o desvio padrão de médias calculadas a partir de uma amostra de 
# tamanho n𝑛, tirada de uma população.

# Digamos que você obtenha 10000 amostras de uma população qualquer com um tamanho de amostra de n = 2.
# Então calculamos as médias de cada uma dessas amostras (teremos 10000 médias calculadas).
# A equação acima informa que, com um número de amostras grande o suficiente, o desvio padrão das médias 
# da amostra pode ser aproximado usando esta fórmula: sd(amostra) / sqrt(nrow(amostra))
  
# Deve ser intuitivo que o seu desvio padrão das médias da amostra será muito pequeno, 
# ou em outras palavras, as médias de cada amostra terão muito pouca variação.

# Com determinadas condições de inferência (nossa amostra é aleatória, normal, independente), 
# podemos realmente usar esse cálculo de desvio padrão para estimar o desvio padrão de nossa população. 
# Como isso é apenas uma estimativa, é chamado de erro padrão. A condição para usar isso como 
# uma estimativa é que o tamanho da amostra n é maior que 30 (dado pelo teorema do limite central) 
# e atende a condição de independência n <= 10% do tamanho da população.
df_amostral
summary(df_amostra1)

# Erro padrão
erro_padrao_amostra1 = sd(df_amostra1$arr_delay) / sqrt(nrow(df_amostra1))
erro_padrao_amostra1

# Limites inferior e superior
# 1.96 é o valor de z score para 95% de confiança
lower_1 = mean(df_amostra1$arr_delay) - 1.96 * erro_padrao_amostra1
upper_1 = mean(df_amostra1$arr_delay) + 1.96 * erro_padrao_amostra1

# Intervalo de confiança
IC_1 = c(lower_1, upper_1)
IC_1

# Exercício 5 - Calcule o intervalo de confiança (95%) da amostra2
# Erro padrão
erro_padrao_amostra2 = sd(df_amostra2$arr_delay, na.rm = TRUE) / sqrt(nrow(df_amostra2))
erro_padrao_amostra2

# Limites inferior e superior
# 1.96 é o valor de z score para 95% de confiança
lower_2 = mean(df_amostra2$arr_delay) - 1.96 * erro_padrao_amostra2
upper_2 = mean(df_amostra2$arr_delay) + 1.96 * erro_padrao_amostra2

# Intervalo de confiança
IC_2 = c(lower_2, upper_2)
IC_2

# Exercício 6 - Crie um plot Visualizando os intervalos de confiança criados nos itens anteriores
# Dica: Use o geom_point() e geom_errorbar() do pacote ggplot2
?ggplot
?geom_point

toPlot = summarise(group_by(df_amostral, sample_id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1, IC_1[1], IC_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 2, IC_1[2], IC_2[2]))
ggplot(toPlot, aes(x = sample_id, y = mean, colour = sample_id)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1)

# Exercício 7 - Podemos dizer que muito provavelmente, as amostras vieram da mesma população? 
# Por que?
  #Sim, porque pelo gráfico pode se notar q a maioria dos valores fazem parte da mesma população  


# Exercício 8 - Crie um teste de hipótese para verificar se os voos da Delta Airlines (DL)
# atrasam mais do que os voos da UA (United Airlines)

# H0 e H1 devem ser mutuamente exclusivas.

# H0 = Não há diferença significativa entre os atrasos da DL e UA (dff média de atrasos = 0)
# H1 = DL atrasa mais (dff médias > 0)
#Criar amostra
df_amostra1
df_amostra2


#Calcula erro padrão e média
erro_padrao_amostra1
erro_padrao_amostra2

media1 <- mean(df_amostra1$arr_delay)
media2 <- mean(df_amostra1$arr_delay)

#Limites Inferiores e Superiores
lower_1
lower_2

upper_1
upper_2

IC_1
IC_2

#Teste t
t.test(df_amostra1$arr_delay, df_amostra2$arr_delay, alternative = "greater")

#Valor p > 0.05 rejeitamos o H0

