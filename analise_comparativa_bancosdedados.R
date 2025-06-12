#Carregando pacotes necessários
library(ggplot2)

# Importando os bancos de dados do módulo de normalização dos bancos 
source("normalizacao_bancodedados.R")

# Análise 1 - salários por nível de experiência -- banco Gringo
bancoGringo$nivel_de_experiencia <- factor(bancoGringo$nivel_de_experiencia, 
                            levels = c("Entry-level", "Mid-level", "Senior-level", "Executive-level")) #Organiza os níveis dos cargos 
grafico_01 <- ggplot(data = bancoGringo, mapping = aes(x = nivel_de_experiencia, y = salario_usdt)) +
  geom_boxplot(fill = "darkgreen") + 
  labs(title = "Distribuição de salários por nível de experiência no exterior",
       x = "Níveis de experiência", 
       y = "Salário anual ($)") +
  theme_bw()

# Análise 2 - salários por nível de experiência -- banco BR
grafico_02 <- ggplot(data = bancoBR, mapping = aes(x = nivel_de_experiencia, y = renda_anual_usd))+
  geom_boxplot(fill = "blue") +
  labs(title = "Distribuição de salários por nível de experiência no Brasil",
       x = "Níveis de experiências",
       y = "Salário anual ($)") + 
  theme_bw()

# Análise 3 - Mapa da distribuição dos salários no Brasil
library(geobr)
mapa_estados <- read_state(year = 2020)

#Calculando a média salarial
salario_estado <- bancoBR %>%
  group_by(Estado_onde_vive) %>%
  summarise(media_salarial = mean(renda_anual_brl, na.rm = TRUE)) %>%
  ungroup()

# Verificando se os nomes batem antes de juntar
mapa_estado_salario <- mapa_estados %>%
  left_join(salario_estado, by = c("name_state" = "Estado_onde_vive"))

    # Fazendo o join com base nos nomes dos estados
mapa_salario <- mapa_estados %>%
  left_join(salario_estado, by = c("name_state" = "Estado_onde_vive"))
mapa <- ggplot(data = mapa_salario) +
  geom_sf(aes(fill = media_salarial), color = "grey") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Salário médio (BRL)") +
  theme_minimal() +
  labs(title = "Mapa de calor do salário médio por estado no Brasil") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


# Analise 4 - Tabela bivariada --> medidas resumo do salário por nível (banco BR)
tabela04 <- bancoBR %>%
  group_by(nivel_de_experiencia) %>%
  summarise(
    media = mean(renda_anual_usd, na.rm = TRUE),
    mediana = median(renda_anual_usd, na.rm = TRUE),
    desvio_padrao = sd(renda_anual_usd, na.rm = TRUE),
    minimo = min(renda_anual_usd, na.rm = TRUE),
    maximo = max(renda_anual_usd, na.rm = TRUE),
    n = n()
  )

# Análise 5 - Tabela bivariada --> medidas resumo do salário por nível (banco gringo)
tabela05 <-bancoGringo %>%
  group_by(nivel_de_experiencia) %>% 
  summarise(
    media = mean(salario_usdt, na.rm = TRUE),
    mediana = median(salario_usdt, na.rm = TRUE),
    desvio_padrao = sd(salario_usdt, na.rm = TRUE),
    minimo = min(salario_usdt, na.rm = TRUE),
    maximo = max(salario_usdt, na.rm = TRUE),
    n = n()
  )

# Análise 6 - Tabela bivariada --> Cargo X Salário (banco BR)
tabela06 <- bancoBR %>%
  group_by(cargo)%>%
  summarise(
    media_salarial = mean(renda_anual_usd, na.rm = TRUE)
  )


# Análise 7 - Tabela bivariada --> Cargo X Salário (banco gringo)
tabela07 <- bancoGringo %>%
  group_by(cargo) %>%
  summarise(
    media_salarial = mean(salario_usdt, na.rm = TRUE)
  )

# Análise 8 - Salários médios por tamanho da empresa (banco Gringo)
bancoGringo$tamanho_da_empresa <- factor(bancoGringo$tamanho_da_empresa, 
                                           levels = c("Small", "Medium", "Large")) #Organiza os níveis dos tamanhos das empresas 

grafico_08 <- ggplot(data = bancoGringo, mapping = aes(x = tamanho_da_empresa , y = salario_usdt ))+
  geom_boxplot(fill = "darkred") +
  labs(title = "Salários médios para cada tamanho de empresa (anual)",
       x = "Tamanho da empresa",
       y = "Salário médio ($)") + 
  theme_bw()

# Análise 9 - Salários médios por tamanho da empresa (banco BR)

#Organizando em ordem decrescente de tamanho de empresa
bancoBR$tamanho_da_empresa <- factor(bancoBR$tamanho_da_empresa,
                                     levels = c("de 1 a 5", "de 6 a 10", "de 11 a 50", "de 51 a 100",
                                                "de 101 a 500", "de 501 a 1.000", "de 1.001 a 3.000",
                                                "Acima de 3.000", "NA"))

#Gráfico da análise 9
grafico_09 <- ggplot(data = bancoBR, mapping = aes(x = tamanho_da_empresa, y = renda_anual_usd)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Salários médios por tamanho de empresa (anual)",
       x = "Tamanho da empresa",
       y = "Salário médio ($)") +
  theme_bw()


# Análise 10 - salário médio dos países
medias <-bancoGringo %>%
  group_by(local_da_empresa) %>% 
  summarise(
    media = mean(salario_usdt, na.rm = TRUE),
  )

#Gráficos de barras da distribuição dos salários médios por países 

# Análise 10.1 -- Top 10 maiores salários por país 
top10 <- medias %>%
  top_n(10, media)

grafico_10_1<- ggplot(top10, aes(x = media, y = reorder(local_da_empresa, +media))) + 
  geom_bar(stat = "identity", fill = "midnightblue") +
  labs(title = "10 maiores salários por país",
       x = "País",
       y = "Salários anuais médios ($)") +
  theme_bw() 

# Análise 10.2 -- Top 10 menores salários por país
top_menores10 <- medias %>%
  top_n(-10, media)

grafico_10_2 <- ggplot(top_menores10, aes(x= media, y = reorder(local_da_empresa, +media))) + 
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "10 menores salários por país",
       x = "País",
       y = "Salários anuais médios ($)") +
  theme_bw()

# Análise 10.3 -- Posição geral dos países
grafico_10_3 <- ggplot(medias, aes(x =reorder(local_da_empresa, +media) , y = media , 
                  fill = ifelse(local_da_empresa == "Brazil", "Destaque", "Outros"))) +
  geom_col() +
  scale_fill_manual(values = c("Destaque" = "red", "Outros" = "green")) +
  coord_flip() +
  labs(
    title = "Salários médios com destaque para o Brasil",
    x = "País",
    y = "Salário anual médio ($)"
  ) +
  theme_bw() +
  theme(legend.position = "none")
