# Carregando os dados 
bancoBR23 <- read.csv("stateofdata_2023.csv")
bancoBR24 <-read.csv("stateofdata_2024.csv")
bancoGringo <- read.csv("data_science_salaries.csv")

# Carregando os pacotes necessários 
library(readr)
library(dplyr)

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

# Criando um novo banco de dados com somente os dados de interesse (para os bancos nacionais)
bancoBR23 <- bancoBR23 %>% 
  select(, c(32,30,25,37,36,35,31))
#select("x Faixa salarial", "x Cargo Atual", "x Nível", "x Qual sua situação atual de trabalho", 
 #        "x Numero de Funcionarios", "x Regiao onde mora", "x Setor")

