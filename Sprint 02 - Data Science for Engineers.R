# Instalar pacotes se necessário
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(psych)) install.packages("psych")

# Carregar os pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(psych)

# Verificar se o arquivo existe antes de tentar ler
caminho_arquivo <- "C:/Users/THIISS/Downloads/Dados.xlsx"
if (!file.exists(caminho_arquivo)) {
  stop("O arquivo 'Dados.xlsx' não foi encontrado na pasta Downloads. Por favor, verifique o caminho!")
}

# Importar os dados
dados <- read_excel(caminho_arquivo, sheet = "Dados")

# Verificar os nomes das colunas
print(colnames(dados))

# Ajustar escala para % Concluída (0-100) caso esteja em 0-1
if (max(dados$`% Concluída`, na.rm = TRUE) <= 1) {
  dados <- dados %>% mutate(`% Concluída` = `% Concluída` * 100)
}

## 1. Estatísticas Descritivas Gerais
desc_stats <- psych::describe(dados$`% Concluída`, IQR = TRUE)
print(desc_stats)

## 2. Tabela de Frequência para % Concluída
faixas <- cut(dados$`% Concluída`, 
              breaks = c(0, 25, 50, 75, 100),
              labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
              include.lowest = TRUE)

tabela_freq <- table(faixas)
print("Tabela de Frequência:")
print(tabela_freq)

## Função para calcular moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## 3. Estatísticas por Categoria
stats_categoria <- dados %>%
  group_by(Categoria) %>%
  summarise(
    Média = mean(`% Concluída`, na.rm = TRUE),
    Mediana = median(`% Concluída`, na.rm = TRUE),
    Moda = moda(`% Concluída`),
    Q1 = quantile(`% Concluída`, 0.25, na.rm = TRUE),
    Q3 = quantile(`% Concluída`, 0.75, na.rm = TRUE),
    P10 = quantile(`% Concluída`, 0.10, na.rm = TRUE),
    P75 = quantile(`% Concluída`, 0.75, na.rm = TRUE),
    Variância = var(`% Concluída`, na.rm = TRUE),
    Desvio_Padrão = sd(`% Concluída`, na.rm = TRUE),
    Curtose = psych::kurtosi(`% Concluída`, na.rm = TRUE),
    Assimetria = psych::skew(`% Concluída`, na.rm = TRUE)
  )

print("Estatísticas por Categoria:")
print(stats_categoria)

## 4. Estatísticas por Fase
stats_fase <- dados %>%
  group_by(`Fase (1 a 6)`) %>%
  summarise(
    Média = mean(`% Concluída`, na.rm = TRUE),
    Mediana = median(`% Concluída`, na.rm = TRUE),
    Moda = moda(`% Concluída`),
    Q1 = quantile(`% Concluída`, 0.25, na.rm = TRUE),
    Q3 = quantile(`% Concluída`, 0.75, na.rm = TRUE),
    P10 = quantile(`% Concluída`, 0.10, na.rm = TRUE),
    P75 = quantile(`% Concluída`, 0.75, na.rm = TRUE),
    Variância = var(`% Concluída`, na.rm = TRUE),
    Desvio_Padrão = sd(`% Concluída`, na.rm = TRUE),
    Curtose = psych::kurtosi(`% Concluída`, na.rm = TRUE),
    Assimetria = psych::skew(`% Concluída`, na.rm = TRUE)
  )

print("Estatísticas por Fase:")
print(stats_fase)

## 5. Estatísticas por Condição
stats_condicao <- dados %>%
  group_by(`Condição (sempre, A, B, C)`) %>%
  summarise(
    Média = mean(`% Concluída`, na.rm = TRUE),
    Mediana = median(`% Concluída`, na.rm = TRUE),
    Moda = moda(`% Concluída`),
    Q1 = quantile(`% Concluída`, 0.25, na.rm = TRUE),
    Q3 = quantile(`% Concluída`, 0.75, na.rm = TRUE),
    P10 = quantile(`% Concluída`, 0.10, na.rm = TRUE),
    P75 = quantile(`% Concluída`, 0.75, na.rm = TRUE),
    Variância = var(`% Concluída`, na.rm = TRUE),
    Desvio_Padrão = sd(`% Concluída`, na.rm = TRUE),
    Curtose = psych::kurtosi(`% Concluída`, na.rm = TRUE),
    Assimetria = psych::skew(`% Concluída`, na.rm = TRUE)
  )

print("Estatísticas por Condição:")
print(stats_condicao)

## 6. Análise de Correlação entre Duração e % Concluída
correlacao <- cor(dados$`Duração (dias)`, dados$`% Concluída`, use = "complete.obs")
print(paste("Correlação entre Duração e % Concluída:", round(correlacao, 3)))
