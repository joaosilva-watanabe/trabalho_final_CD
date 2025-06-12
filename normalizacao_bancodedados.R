# Carregando os dados 
bancoBR23 <- read.csv("stateofdata_2023.csv")
bancoBR24 <-read.csv("stateofdata_2024.csv")
bancoGringo <- read.csv("data_science_salaries.csv")

# Carregando os pacotes necessários 
library(dplyr)
library(stringr)

# Excluindo informações não relevantes do banco estrangeiro
bancoGringo <- bancoGringo %>% 
  select(-salary, -salary_currency)

# Renomeando as colunas do banco de dados estrangeiro 
names(bancoGringo) <- c("cargo", "nivel_de_experiencia", "tipo_de_contrato", "modelo_de_trabalho",
                        "data_da_resposta", "local_de_residencia", "salario_usdt",
                        "local_da_empresa", "tamanho_da_empresa")

# Filtrando o banco de dados estrangeiro somente para datas compatíveis com os demais bancos 
bancoGringo <- bancoGringo %>% filter(data_da_resposta == 2023 | data_da_resposta == 2024)

# Adicionando uma coluna com a data da resposta nos dois bancos de dados brasileiros 
bancoBR23$Data_da_resposta <- 2023
bancoBR24$Data_da_resposta <- 2024

#Adiconando uma coluna com o local de residência e da empresa (para essa análise, em termos de países)
bancoBR23$local_da_empresa <- "Brasil"
bancoBR23$local_de_residencia <- "Brasil"
bancoBR24$local_da_empresa <- "Brasil"
bancoBR24$local_de_residencia <- "Brasil"
# Criando novos bancos de dados com somente os dados de interesse (para os bancos nacionais)
bancoBR23 <- bancoBR23 %>% 
  select(, c(23, 32,30,37,36,35, 55,400, 401, 402))
bancoBR24 <- bancoBR24 %>% 
  select(, c(29, 36, 39, 34, 40, 41, 48, 404, 405, 406))

# Renomeando as colunas do banco de dados BR de 2023
names(bancoBR23)<- c("Estado_onde_vive", "tamanho_da_empresa", "tipo_de_contrato", "salario_brl",
                     "nivel_de_experiencia", "cargo","trabalho_remoto_flexivel", "data_da_resposta", 
                     "local_da_empresa", "local_de_residencia")
# Renomeando as colunas do banco de dados BR de 2024
names(bancoBR24)<- c("Estado_onde_vive","tamanho_da_empresa", "cargo", "tipo_de_contrato", "nivel_de_experiencia",
                     "salario_brl", "trabalho_remoto_flexivel", "data_da_resposta", "local_da_empresa",
                     "local_de_residencia")

# Convertendo os salários:

#Criando objetos para armazenar os valores
media_salarial23 <- bancoBR23 %>% 
  select(4)
media_salarial24 <- bancoBR24 %>% 
  select(6)

# Pegando o ponto médio de cada intervalo
media_salarial23 <- media_salarial23 %>%
  mutate(salario_brl = case_when(
    str_detect(salario_brl, "Menos de") ~ 1000 / 2,
    str_detect(salario_brl, "de R\\$") ~ {
      numeros <- str_extract_all(salario_brl, "\\d{1,3}(\\.\\d{3})*")
      valores <- lapply(numeros, function(x) as.numeric(gsub("\\.", "", x)))
      sapply(valores, function(x) x[1] + (x[2] - x[1]) / 2)
    },
    TRUE ~ NA_real_
  ))

media_salarial24 <- media_salarial24 %>%
  mutate(salario_brl = case_when(
    str_detect(salario_brl, "Menos de") ~ 1000 / 2,
    str_detect(salario_brl, "de R\\$") ~ {
      numeros <- str_extract_all(salario_brl, "\\d{1,3}(\\.\\d{3})*")
      valores <- lapply(numeros, function(x) as.numeric(gsub("\\.", "", x)))
      sapply(valores, function(x) x[1] + (x[2] - x[1]) / 2)
    },
    TRUE ~ NA_real_
  ))

#Fazendo a renda anual
media_salarial23 <- media_salarial23 %>%
  mutate(renda_anual_brl = salario_brl * 12) %>%
  select(-salario_brl)

media_salarial24 <- media_salarial24 %>%
  mutate(renda_anual_brl = salario_brl * 12) %>%
  select(-salario_brl)

#Convertendo esses valores baseado na média da cotação do dólar nos respectivos anos 
media_salarial23 <- media_salarial23 %>%
  mutate(renda_anual_usd = renda_anual_brl / 4.99)

media_salarial24 <- media_salarial24 %>%
  mutate(renda_anual_usd = renda_anual_brl / 5.57)

#Unindo os dois bancos de dados brasileiros
bancoBR <- rbind(bancoBR23, bancoBR24)

    # Adicionando os valores de salários
salarios <- rbind(media_salarial23, media_salarial24)
bancoBR <- cbind(bancoBR, salarios)

# Tratando valores vazios
bancoBR[bancoBR == ''] <- NA

# Cálculo do salário médio por estado
salario_estado <- bancoBR %>%
  group_by(Estado_onde_vive) %>%
  summarise(media_salarial = mean(renda_anual_brl, na.rm = TRUE))

# Formatando os valores para Estado 
bancoBR <- bancoBR %>%
  mutate(Estado_onde_vive = str_trim(str_remove(Estado_onde_vive, "\\s*\\(.*\\)")))




