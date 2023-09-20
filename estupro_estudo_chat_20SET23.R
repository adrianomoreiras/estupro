#Bibliotecas
library(plm)
library(stargazer)
library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(corrplot)
library(car)
library(AER)
library(glmulti)
library(tseries)
library(ggplot2)

options(scipen = 999)

#base de dados
df <- read_excel("dados_est.xlsx", sheet = "dados")



#tratamento
df1 <- df %>% select(Micro, Ano, Taxa, Den_demo, T_MC, T_MEI, T_MEF, T_MEM,
                     T_DC, T_DEI, T_DEF, T_DEM, T_PO, T_POASS, Media_PIB_Cap, Media_QSMM, Media_SOR, PIB) #%>% 
 # filter(Ano==c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

df1$Taxa <- log(df1$Taxa)
df1$Den_demo <- log(df1$Den_demo)
df1$T_MEI <- log(df1$T_MEI)
df1$T_MEF <- log(df1$T_MEF)
df1$T_MEM <- log(df1$T_MEM)
df1$T_MC <- log(df1$T_MC)
df1$T_DC <- log(df1$T_DC)
df1$T_DEI <- log(df1$T_DEI)
df1$T_DEF <- log(df1$T_DEF)
df1$T_DEM <- log(df1$T_DEM)
df1$T_PO <- log(df1$T_PO)
df1$T_POASS <- log(df1$T_POASS)
df1$Media_PIB_Cap <- log(df1$Media_PIB_Cap)
df1$Media_QSMM <- log(df1$Media_QSMM)
df1$Media_SOR <- log(df1$Media_SOR)
df1$PIB <- log(df1$PIB)

#write.table(df1,"eed.csv", dec = ",", sep = " ")

correl <- cor(df1 %>% select(Taxa, T_MEI, T_MEF, T_MEM,
                             T_DEI, T_DC,
                             Media_PIB_Cap, 
                             T_POASS, PIB))

#Taxa ~ T_DC + T_DEI + T_MEI + T_MEM + T_POASS + PIB
print(correl)

corrplot(correl, type = "upper", order = 'FPC')

# Defina o painel
pdata <- pdata.frame(df1, index = c("Micro", "Ano"))

f <- as.formula(Taxa ~ T_DC + T_DEI + 
                  T_MEI + T_MEM + 
                  T_POASS + PIB)


w <- plm(f, data = pdata, model = "within")
r <- plm(f, data = pdata, model = "random")
p <- plm(f, data = pdata, model = "pooling")

# Esolha dos estimadores

#Para o teste de Poolability (Chow)(w x p): 
  pFtest(w, p)

#Para o teste de Breusch e Pagan (r x p): 
  plmtest(p, type="bp")

  #Para o teste de Hausman (w x r): 
  phtest(w, r)

#autocorrelação
pbgtest(w, order = 1)

#Homocedasticidade dos resíduos
bptest(w)

#Testando dependência transversal
pcdtest(w, test="cd")

#Normalidade dos resíduos
shapiro.test(w$residuals)

summary(w)

#ajuste de autocorrelação e dependência transversal
coeftest(w, vcov=vcovDC(w, type="HC3"))


#Testando raízes unitárias
adf.test(df1$Taxa, k=2)


#####

library(dplyr)

pdata_avg <- pdata %>%
  group_by(Ano) %>%
  summarize(avg_Taxa = mean(Taxa, na.rm = TRUE))

library(ggplot2)

ggplot(pdata, aes(x=Ano, y=Taxa)) + 
  geom_line(aes(group=Micro), alpha=0.3) +  # Linhas finas para cada microrregião
  geom_line(data=pdata_avg, aes(x=Ano, y=avg_Taxa), size=1, color="red") +  # Linha grossa para a média
  labs(title="Taxa de estupro por ano", x="Ano", y="Taxa") +
  theme_minimal()

ggplot(pdata_avg, aes(x=Ano, y=avg_Taxa)) + 
  geom_line(size=1, color="red") +
  labs(title="Média da taxa de estupro por ano", x="Ano", y="Taxa") +
  theme_minimal()

microrregioes_selecionadas <- c("Barretos", "Capão Bomito", "Itanhaém", "Itapetininga", 
                                "Registro", "São Paulo")

pdata_selecionado <- pdata %>% 
  filter(Micro %in% microrregioes_selecionadas)

graf <- ggplot(pdata_selecionado, aes(x=Ano, y=Taxa)) + 
  geom_line(aes(group=Micro), alpha=0.5) +  
  geom_line(data=pdata_avg, aes(x=Ano, y=avg_Taxa), size=1, color="red") +
  labs(title="Taxa de estupro por ano", x="Ano", y="Taxa") +
  theme_minimal()


#library(plotly)
#plotly(graf)

# Adicione variáveis dummy para cada ano

pdata$year2011 <- as.numeric(pdata$Ano == 2011)
pdata$year2012 <- as.numeric(pdata$Ano == 2012)
pdata$year2013 <- as.numeric(pdata$Ano == 2013)
pdata$year2014 <- as.numeric(pdata$Ano == 2014)
pdata$year2015 <- as.numeric(pdata$Ano == 2015)
pdata$year2016 <- as.numeric(pdata$Ano == 2016)
pdata$year2017 <- as.numeric(pdata$Ano == 2017)
pdata$year2018 <- as.numeric(pdata$Ano == 2018)
pdata$year2019 <- as.numeric(pdata$Ano == 2019)
pdata$year2020 <- as.numeric(pdata$Ano == 2020)


# Estime o modelo com as variáveis dummy de ano
w_year_dummies <- plm(Taxa ~ T_DC + T_DEI + 
                        T_MEI + T_MEM + 
                        T_POASS + PIB + 
                        year2012 + 
                        year2013 + year2014 + 
                        year2015 + year2016 +
                        year2017 + year2018 +
                        year2019 + year2020, 
                      data=pdata, model="within")
summary(w_year_dummies)

# Model
w_year_dummies_2020_ref <- plm(Taxa ~ T_DC + T_DEI + T_MEI + T_MEM + T_POASS + PIB + 
                                 year2011 + year2012 + year2013 + year2014 + year2015 + 
                                 year2016 + year2017 + year2018 + year2019, 
                               data = pdata, model = "within")

summary(w_year_dummies_2020_ref)

#autocorrelação
pbgtest(w_year_dummies_2020_ref, order = 1)

#Homocedasticidade dos resíduos
bptest(w_year_dummies_2020_ref)

#Testando dependência transversal
pcdtest(w_year_dummies_2020_ref, test="cd")

#Normalidade dos resíduos
shapiro.test(w_year_dummies_2020_ref$residuals)

#ajuste de autocorrelação e dependência transversal
coeftest(w_year_dummies_2020_ref, vcov=vcovDC(w_year_dummies_2020_ref, type="HC3"))


fixef(w)


#library(plm)

# Modelo de efeitos fixos
model_within <- plm(Taxa ~ T_DC + T_DEI + T_MEI + T_MEM + T_POASS + PIB, data=pdata, model="within")

# Modelo OLS (pooled)
model_pooled <- plm(Taxa ~ T_DC + T_DEI + T_MEI + T_MEM + T_POASS + PIB, data=pdata, model="pooling")

# Realizar o teste F
ftest <- pFtest(model_within, model_pooled)
print(ftest)

########

