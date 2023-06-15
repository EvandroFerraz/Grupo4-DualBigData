# Instalação da biblioteca para arquivo .xlsx
install.packages("readxl")

# Selecionar a biblioteca
library(readxl)

# Importar a base de dados
db <- read_xlsx(path="/Users/yujiwatanabe/Documents/dados/dadosAtualizados.xlsx")

# Apresentar a base de dados
db

# Filtrar os dados da amostra A
amostraA <- db[db$ANO_VENDA < "2020", ]

# Filtrar os dados da amostra B
amostraB <- db[db$ANO_VENDA >= "2020", ]

# Cálculo da média
mediaA <- mean(amostraA$QTD_UNIDADE_FARMACOTECNICA)
mediaB <- mean(amostraB$QTD_UNIDADE_FARMACOTECNICA)

# Cálculo do desvio padrão
desvioA <- sd(amostraA$QTD_UNIDADE_FARMACOTECNICA)
desvioB <- sd(amostraB$QTD_UNIDADE_FARMACOTECNICA)

# Cálculo do valor de nA e nB
nA <- length(amostraA$QTD_UNIDADE_FARMACOTECNICA)
nB <- length(amostraB$QTD_UNIDADE_FARMACOTECNICA)

# Cálculo do valor do teste t
t <- (mediaA - mediaB) / sqrt((desvioA^2 / nA) + (desvioB^2 / nB))

# Cálculo do valor crítico xc
xc <- qt(0.05, df = min(nA - 1, nB - 1))

# Resultado do teste t
if (t > xc) {
  cat("Ha foi confirmada (rejeitar H0)")
} else {
  cat("Ha não foi confirmada (aceitar H0)")
}
