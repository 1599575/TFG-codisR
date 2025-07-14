
# REGRESSIÓ LOGÍSTICA A PARTIR DE PCA


###### LLIBRERIES ######

library(dplyr)
library(broom)
library(ResourceSelection)
library(pROC)



###### DADES ######

base_dades <- readRDS("ruta_arxiu")

puntuacions <- readRDS("ruta_arxiu")

dades_regressio <- base_dades %>%
  select(COD_FAQCIL, ALTERAT_V1, SEXE, Edat_V1, CA_tabac_v1, Consum_alcohol, Test_CLASSAF_V1c, IMC_V1) %>%
  left_join(
    puntuacions %>%
      as.data.frame() %>%
      mutate(COD_FAQCIL = as.character(rownames(.))),
    by = "COD_FAQCIL") %>%
  rename(Patro1 = V1, Patro2 = V2, Patro3 = V3)



###### REGRESSIÓ LOGÍSTICA, RESULTATS I AVALUACIÓ D'AJUST ######

# Model complet de regressió logística
model_complet <- glm(ALTERAT_V1 ~ Patro1 + Patro2 + Patro3 + SEXE + Edat_V1 + CA_tabac_v1 + Consum_alcohol + Test_CLASSAF_V1c + IMC_V1,
                     family = binomial, data = dades_regressio)

# Resultats del model (odds ratio, p-valor i intervals de confiança del 95%)
resultats_model <- broom::tidy(model_complet, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(., 3)),
         p.value = format.pval(p.value, digits = 2))

# Bondat d'ajust (Hoslem test)
hoslem <- ResourceSelection::hoslem.test(model_complet$y, fitted(model_complet), g = 10)

# Corba ROC i AUC
corba_roc <- roc(response = dades_regressio$ALTERAT_V1,
                 predictor = predict(model_complet, type = "response"))
auc <- auc(roc_curve) 



###### ANÀLISI SEPARANT PER SEXE ######

# Dividim la base de dades entre homes (0) i dones (1)
dades_homes <- subset(dades_regressio, SEXE == 0)
dades_dones <- subset(dades_regressio, SEXE == 1)

# Model en homes/dones de regressió logística
model_homes <- glm(ALTERAT_V1 ~ Patro1 + Patro2 + Patro3 + Edat_V1 + CA_tabac_v1 + Consum_alcohol + Test_CLASSAF_V1c + IMC_V1,
                   family = binomial,
                   data = dades_homes)
model_dones <- glm(ALTERAT_V1 ~ Patro1 + Patro2 + Patro3 + Edat_V1 + CA_tabac_v1 + Consum_alcohol + Test_CLASSAF_V1c + IMC_V1,
                   family = binomial,
                   data = dades_dones)

# Resultats del model en homes/dones (odds ratio, p-valor i intervals de confiança del 95%)
resultats_homes <- broom::tidy(model_homes, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(., 3)),
         p.value = format.pval(p.value, digits = 2))
resultats_dones <- broom::tidy(model_dones, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(., 3)),
         p.value = format.pval(p.value, digits = 2))
         
# Bondat d'ajust (Hoslem test) pel model en homes/dones
hoslem_homes <- ResourceSelection::hoslem.test(model_homes$y, fitted(model_homes), g = 10)
hoslem_dones <- ResourceSelection::hoslem.test(model_dones$y, fitted(model_dones), g = 10)

# Corba ROC i AUC pel model en homes/done
roc_homes <- roc(response = dades_homes$ALTERAT_V1,
                 predictor = predict(model_homes, type = "response"))
auc_homes <- auc(roc_homes) 
roc_dones <- roc(response = dades_dones$ALTERAT_V1,
                 predictor = predict(model_dones, type = "response"))
auc_dones <- auc(roc_dones) 
