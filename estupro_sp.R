#Bibliotecas
library(plm)
library(stargazer)
library(readxl)
library(dplyr)

#base de dados
df <- read_excel("dados_est.xlsx", sheet = "dados")

#tratamento
df1 <- df %>% select(Micro, Ano, Taxa, Den_demo, T_MC, T_MEI, T_MEF, T_MEM,
              T_DC, T_DEI, T_DEF, T_DEM, T_PO, Media_PIB_Cap, Media_QSMM)

#Correlação
correl <- cor(df %>% select(Taxa, Den_demo, T_MC, T_MEI, T_MEF, T_MEM,
              T_DC, T_DEI, T_DEF, T_DEM, T_PO, Media_PIB_Cap, Media_QSMM))

print(correl)

#modelo

form <- Taxa ~ Den_demo + T_MC + T_MEI + T_MEF + T_MEM +
T_DC + T_DEI + T_DEF + T_DEM + T_PO + Media_PIB_Cap + Media_QSMM

#Estimadores
ls
