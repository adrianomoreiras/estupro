library(plm)
library(stargazer)
library(readxl)
library(dplyr)
library(lmtest)
library(corrplot)
library(sandwich)
library(car)
library(panelView)
library(performance)
library(lme4)

dataset <- read_excel("dados_est.xlsx", sheet = "dados")

form <- log(Taxa) ~ log(T_MC) + log(T_MEI) + log(T_MEF) + log(T_MEM) +
  log(T_DC) + log(T_DEI) + log(T_DEF) + log(T_DEM) +
  log(T_POASS) + log(Media_QSMM) + log(Den_demo)+
  log(T_PO) + log(Media_SOR) + log(PIB) + log(Media_PIB_Cap)



# Efeitos fixos
fixed_effects <- plm(form, data = dataset, model = "within", index = c("Micro", "Ano"))#, vcov. = vcovHC(fixed_effects, method = "arellano"))



check_autocorrelation(fixed_effects)

check_heteroscedasticity(fixed_effects)


#biblioteca lme4
model_fixo_lme4 <- lmer(log(Taxa) ~ log(T_MC) + log(T_MEI) + log(T_MEF) + log(T_MEM) +
                          log(T_DC) + log(T_DEI) + log(T_DEF) + log(T_DEM) +
                          log(T_POASS) + log(Media_QSMM) + log(Den_demo)+
                          log(T_PO) + log(Media_SOR) + log(PIB) + log(Media_PIB_Cap)+(1|Micro),
                        data = dataset)

summary(model_fixo_lme4)
check_collinearity(model_fixo_lme4)
check_autocorrelation(model_fixo_lme4)
check_heteroscedasticity(model_fixo_lme4)
check_normality(model_fixo_lme4)
check_distribution(model_fixo_lme4)
check_outliers(model_fixo_lme4)
check_predictions(model_fixo_lme4)
#check_model(model_fixo_lme4)
check_predictions(model_fixo_lme4,type = "discrete_dots")
check_predictions(model_fixo_lme4,type = "discrete_interval")
check_predictions(model_fixo_lme4,type = "discrete_both")

plot(model_fixo_lme4)

check_model(model_fixo_lme4)


# Efeitos aleatórios
random_effects <- plm(form, data = dataset, model = "random")
# Pooled
pooled <- plm(form, data = dataset, model = "pooling")

stargazer(fixed_effects, random_effects, pooled, type = "text", column.labels = c("Efeitos fixos", 
                                                                                  "Efeitos aleatórios",
                                                                                  "Empilhados"))

bptest(fixed_effects, studentize = FALSE)
bptest(random_effects, studentize = FALSE)


coeftest(fixed_effects, vcov. = vcovHC(fixed_effects, method = "arellano"))
summary(fixed_effects)


