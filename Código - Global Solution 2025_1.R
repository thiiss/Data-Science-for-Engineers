#Importar os dados (corrigindo o caminho)
Dados <- readxl::read_excel("C:/Users/THIISS/OneDrive/Documentos/Planilha02.xlsx")

#Verificar a estrutura dos dados
str(Dados)  

#Corrigir nomes das colunas
names(Dados) <- c("data", "vento")

#Verificar conversão
print(summary(Dados$vento))

#Conversão
converter_para_numerico <- function(x) {
  # Remove espaços extras, substitui vírgulas por pontos
  x_limpo <- gsub(",", ".", trimws(as.character(x)))
  # Converte para numérico
  as.numeric(x_limpo)
}

# Aplicar a conversão
Dados$vento <- converter_para_numerico(Dados$vento)

# Verificar resultado
if(all(is.na(Dados$vento))) {
  stop("Todos os valores resultaram em NA! Verifique os dados originais.")
}

#Declarar Vetores
datas <- Dados$data
vento <- Dados$vento

#Calcular Media & Média
media_vento <- mean(vento, na.rm = TRUE)
mediana_vento <- median(vento, na.rm = TRUE)

#Mostrar Resultados da Media & Média
cat("Média do vento:", round(media_vento, 2), "km/h\n")
cat("Mediana do vento:", mediana_vento, "km/h\n")

#Calcular Quartis
Dados$vento <- as.numeric(Dados$vento)
quartis <- quantile(Dados$vento, probs = c(0.25, 0.75), na.rm = TRUE)

#Extrair Q1 e Q3
Q1 <- quartis[["25%"]]
Q3 <- quartis[["75%"]]

#Mostrar Resultado Quartis
cat("Primeiro Quartil (Q1):", Q1, "km/h\n")
cat("Terceiro Quartil (Q3):", Q3, "km/h\n")

#Calcular P10 e P90
percentis <- quantile(Dados$vento, probs = c(0.10, 0.90), na.rm = TRUE)

#Extrair os Valores
P10 <- percentis[["10%"]]
P90 <- percentis[["90%"]]

#Mostrar ResultadoS P10 e P90
cat("10º Percentil (P10):", P10, "km/h\n")
cat("90º Percentil (P90):", P90, "km/h\n")

#Calcular a Variância 
variancia <- var(Dados$vento, na.rm = TRUE)

#Mostrar Resultado variância
cat("Variância da velocidade do vento:", variancia, "km²/h²\n")

#Calcular o desvio padrão (com tratamento para NA)
desvio_padrao <- sd(Dados$vento, na.rm = TRUE)

#Mostrar o resultado Desvio padrão
cat("Desvio Padrão da velocidade do vento:", round(desvio_padrao, 2), "km/h\n")

#Calcular Coeficiente de Variação (em porcentagem)
CV <- (desvio_padrao / media_vento) * 100

#Mostrar Resultado Coeficiente de Variação
cat("Coeficiente de Variação:", round(CV, 2), "%\n")

