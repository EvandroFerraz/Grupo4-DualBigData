#instalação da biblioteca para arquivo .xlsx
install.packages("readxl")
library(dplyr)

#selecionar a biblioteca
library(readxl)

#importa a base de dados
db <- read_xlsx(path="/Users/yujiwatanabe/Documents/dados/dadosAtualizados.xlsx")

#apresentar a base de dados
db

#Filtrar os dados da amostra A
amostraA <- db[db$ANO_VENDA<"2020",]

#Filtrar os dados da amostra B
amostraB<-db[db$ANO_VENDA>="2020",]

#1 conjunto de dados
str(db)

#-------------------------------
# 3 - Descrever cada variável

# se a variável for qualitativa, fazer a tabela de frequências de cada valor
table(db$UF_VENDA, db$MUNICIPIO_VENDA, db$PRINCIPIO_ATIVO, db$UNIDADE_MEDIDA_PRINCIPIO_ATIVO,
      db$TIPO_UNIDADE_FARMACOTECNICA, db$SEXO)
#-------------------------------
# se a variável for quantitativa, mostrar as medidas de posição e de dispersão

#Ano da venda do medicamento
summary(db$ANO_VENDA)
var(db$ANO_VENDA)
sd(db$ANO_VENDA)

#Mês da venda do medicamento
summary(db$MES_VENDA)
var(db$MES_VENDA)
sd(db$MES_VENDA)

#Quantidade de unidades farmacotécnicas vendidas
summary(db$QTD_UNIDADE_FARMACOTECNICA)
var(db$QTD_UNIDADE_FARMACOTECNICA)
sd(db$QTD_UNIDADE_FARMACOTECNICA)

#Quantidade de unidades farmacotécnicas vendidas
summary(db$IDADE)
var(db$IDADE)
sd(db$IDADE)

#Quantidade de princípio ativo por unidade farmacotécnica
summary(db$QTD_ATIVO_POR_UNID_FARMACOTEC)
var(db$QTD_ATIVO_POR_UNID_FARMACOTEC)
sd(db$QTD_ATIVO_POR_UNID_FARMACOTEC)

library(ggplot2)

# Histograma
ggplot(db, aes(x = ANO_VENDA)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  labs(x = "Ano de Venda", y = "Contagem") +
  ggtitle("Histograma do Ano de Venda")

# Gráfico de Densidade
ggplot(db, aes(x = ANO_VENDA)) +
  geom_density(fill = "orange", color = "black") +
  labs(x = "Ano de Venda", y = "Densidade") +
  ggtitle("Gráfico de Densidade do Ano de Venda")

# Gráfico de Barras
ggplot(db, aes(x = factor(ANO_VENDA))) +
  geom_bar(fill = "darkblue", color = "black") +
  labs(x = "Ano de Venda", y = "Contagem") +
  ggtitle("Gráfico de Barras do Ano de Venda")

#-------------------------------
db_plot <- data.frame(db)

db_plot$MES_VENDA[db_plot$MES_VENDA == 1] <- "Janeiro"
db_plot$MES_VENDA[db_plot$MES_VENDA == 2] <- "Fevereiro"
db_plot$MES_VENDA[db_plot$MES_VENDA == 3] <- "Março"
db_plot$MES_VENDA[db_plot$MES_VENDA == 4] <- "Abril"
db_plot$MES_VENDA[db_plot$MES_VENDA == 5] <- "Maio"
db_plot$MES_VENDA[db_plot$MES_VENDA == 6] <- "Junho"
db_plot$MES_VENDA[db_plot$MES_VENDA == 7] <- "Julho"
db_plot$MES_VENDA[db_plot$MES_VENDA == 8] <- "Agosto"
db_plot$MES_VENDA[db_plot$MES_VENDA == 9] <- "Setembro"
db_plot$MES_VENDA[db_plot$MES_VENDA == 10] <- "Outubro"
db_plot$MES_VENDA[db_plot$MES_VENDA == 11] <- "Novembro"
db_plot$MES_VENDA[db_plot$MES_VENDA == 12] <- "Dezembro"
# Histograma
ggplot(db, aes(x = MES_VENDA)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  labs(x = "Mes de Venda", y = "Contagem") +
  ggtitle("Histograma do Mes de Venda")

# Gráfico de Densidade
ggplot(db, aes(x = MES_VENDA)) +
  geom_density(fill = "orange", color = "black") +
  labs(x = "Mes de Venda", y = "Densidade") +
  ggtitle("Gráfico de Densidade do Mes de Venda")

plot18 <- db[db$ANO_VENDA == "2018",]
plot19 <- db[db$ANO_VENDA == "2019",]
plot20 <- db[db$ANO_VENDA == "2020",]
plot21 <- db[db$ANO_VENDA == "2021",]

# Gráfico de Barras
ggplot(plot20, aes(x = factor(MES_VENDA))) +
  geom_bar(fill = "darkblue", color = "black") +
  labs(x = "Mes de Venda", y = "Contagem") +
  ggtitle("Gráfico de Barras do Mes de Venda - 2020")

#-------------------------------

# Histograma
ggplot(db, aes(x = QTD_UNIDADE_FARMACOTECNICA)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  labs(x = "Ano de Venda", y = "Contagem") +
  ggtitle("Histograma do Quantidade de unidades farmacotécnicas vendidas")

# Gráfico de Densidade
ggplot(db, aes(x = QTD_UNIDADE_FARMACOTECNICA)) +
  geom_density(fill = "orange", color = "black") +
  labs(x = "Ano de Venda", y = "Densidade") +
  ggtitle("Gráfico de Quantidade de unidades farmacotécnicas vendidas")

# Gráfico de Barras
ggplot(db, aes(x = factor(QTD_UNIDADE_FARMACOTECNICA))) +
  geom_bar(fill = "darkblue", color = "black") +
  labs(x = "Ano de Venda", y = "Contagem") +
  ggtitle("Gráfico de Quantidade de unidades farmacotécnicas vendidas")
#-------------------------------

# Histograma
ggplot(db, aes(x = IDADE)) +
  geom_histogram(binwidth = 1, fill = "yellow", color = "black") +
  labs(x = "Ano de Idade", y = "Contagem") +
  ggtitle("Histograma do Ano de Idade")

# Gráfico de Densidade
ggplot(db, aes(x = IDADE)) +
  geom_density(fill = "orange", color = "black") +
  labs(x = "Ano de Idade", y = "Densidade") +
  ggtitle("Gráfico de Densidade do Ano de Idade")

# Gráfico de Barras
ggplot(db, aes(x = factor(IDADE))) +
  geom_bar(fill = "darkblue", color = "black") +
  labs(x = "Ano de Idade", y = "Contagem") +
  ggtitle("Gráfico de Barras do Ano de Idade")

#-------------------------------

library(plotly)

# Histograma
plot_ly(db, x = ~QTD_ATIVO_POR_UNID_FARMACOTEC, type = "histogram",
        marker = list(color = "yellow", line = list(color = "black")),
        xbins = list(size = 1),
        xaxis = list(title = "Ano de quantidade de ativo por unidade farmacotécnica"),
        yaxis = list(title = "Contagem")) %>%
  layout(title = "Histograma do Ano da quantidade de ativo por unidade farmacotécnica",
         bargap = 0.1,
         barmode = "overlay")

# Gráfico de Densidade
plot_ly(db, x = ~QTD_ATIVO_POR_UNID_FARMACOTEC, type = "histogram", histnorm = "density",
        marker = list(color = "orange", line = list(color = "black")),
        xaxis = list(title = "Ano de quantidade de ativo por unidade farmacotécnica"),
        yaxis = list(title = "Densidade")) %>%
  layout(title = "Gráfico de Densidade do Ano da quantidade de ativo por unidade farmacotécnica",
         showlegend = FALSE)

# Gráfico de Barras
plot_ly(db, x = ~factor(QTD_ATIVO_POR_UNID_FARMACOTEC), type = "histogram",
        marker = list(color = "darkblue", line = list(color = "black")),
        xaxis = list(title = "Ano de quantidade de ativo por unidade farmacotécnica"),
        yaxis = list(title = "Contagem")) %>%
  layout(title = "Gráfico de Barras do Ano da quantidade de ativo por unidade farmacotécnica",
         bargap = 0.1)




#-------------------------------
library(dplyr)

# 4 - Verificar as relações entre variáveis

# criar um conjunto somente com as variáveis quantitativas
db_num <- select_if(db, is.numeric)

#Exclusão coluna idade
db_num <- select(db_num,-c(IDADE,SEXO))

# Calcular a matriz de correlação
correlation_matrix <- cor(db_num)

# Converter a matriz de correlação em um data frame
cor_df <- reshape2::melt(correlation_matrix)

# Plotar o gráfico de correlação usando ggplot2
ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#377EB8", mid = "#FFFFFF", high = "#E41A1C", midpoint = 0,
                       limits = c(-1, 1), guide = "colorbar") +
  labs(x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

