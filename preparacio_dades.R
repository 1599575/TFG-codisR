
# PREPARACIÓ BASE DE DADES


###### LLIBRERIES ######

library(haven)
library(dplyr)



###### DADES ######

medistar_original <- readRDS("ruta_arxiu")

medistar_original <- medistar_original %>% 
    ungroup()

dades_energia <- read_sav("ruta_arxiu")



###### VARIABLES NOVES ######

# Nova columna per tractar el consum d'alcohol
medistar_original$Consum_alcohol <- ifelse(medistar_original$UBE_set_V1 == 0, "No_consumeix", "Consumeix")

# Afegim columna d'energia, que es troba a part, a la resta de variables
medistar_kcal <- merge(medistar_original, dades_energia[, c("n", "kcal_sum")], by.x = "COD_FAQCIL", by.y = "n", all.x = TRUE)



###### VARIABLES D'INTERÈS ######

medistar_bo <- medistar_kcal %>%
  select(COD_FAQCIL, ALTERAT_V1, racio_lactics:racio_begNOalc, Cervesa_set_V1grd, Vi_set_V1grd, Destilades_set_V1grd, kcal_sum, SEXE, Edat_V1, CA_tabac_v1, Consum_alcohol, Test_CLASSAF_V1c, IMC_V1) %>%
  mutate(COD_FAQCIL = as.character(COD_FAQCIL),
         SEXE = as.factor(SEXE),
         Test_CLASSAF_V1c = factor(Test_CLASSAF_V1c, levels = c("inactiva", "Baixa", "Moderada", "Alta")),
         Consum_alcohol = factor(Consum_alcohol, levels = c("No_consumeix", "Consumeix")))



###### MISSINGS I OUTLIERS ######

# Treiem missings
medistar_senseNA <- na.omit(medistar_bo)

# Calculem el 1r i 3r quartils i l'interval interquartílic
Q1 <- quantile(medistar_senseNA$kcal_sum, 0.25)
Q3 <- quantile(medistar_senseNA$kcal_sum, 0.75)
IQR_valor <- IQR(medistar_senseNA$kcal_sum)

# Calculem llindars estadístics pels outliers
llindar_inferior <- Q1 - 1.5 * IQR_valor
llindar_superior <- Q3 + 1.5 * IQR_valor

# Treiem outliers
medistar_clean <- medistar_senseNA[medistar_senseNA$kcal_sum >= llindar_inferior & medistar_senseNA$kcal_sum <= llindar_superior, ] 



###### AJUST ENERGÈTIC ######

mitjana_energia <- mean(medistar_clean$kcal_sum)

base_dades <- medistar_clean

## Làctics
lm_lactics <- lm(racio_lactics ~ kcal_sum, data=base_dades)
base_dades$lactics_adj<-lm_lactics$coefficients[1] + lm_lactics$coefficients[2]*mitjana_energia + lm_lactics$residuals
base_dades$lactics_adj<-ifelse(base_dades$lactics_adj<0, base_dades$racio_lactics/base_dades$kcal_sum, base_dades$lactics_adj)
base_dades$racio_lactics <- NULL

## Brioixeria i dolços
lm_brioix <- lm(racio_brioix ~ kcal_sum, data=base_dades)
base_dades$brioix_adj<-lm_brioix$coefficients[1] + lm_brioix$coefficients[2]*mitjana_energia + lm_brioix$residuals
base_dades$brioix_adj<-ifelse(base_dades$brioix_adj<0, base_dades$racio_brioix/base_dades$kcal_sum, base_dades$brioix_adj)
base_dades$racio_brioix <- NULL

## Verdura
lm_verdura <- lm(racio_verdura ~ kcal_sum, data=base_dades)
base_dades$verdura_adj<-lm_verdura$coefficients[1] + lm_verdura$coefficients[2]*mitjana_energia + lm_verdura$residuals
base_dades$verdura_adj<-ifelse(base_dades$verdura_adj<0, base_dades$racio_verdura/base_dades$kcal_sum, base_dades$verdura_adj)
base_dades$racio_verdura <- NULL

## Farinacis
lm_farinacis <- lm(racio_farinacis ~ kcal_sum, data=base_dades)
base_dades$farinacis_adj<-lm_farinacis$coefficients[1] + lm_farinacis$coefficients[2]*mitjana_energia + lm_farinacis$residuals
base_dades$farinacis_adj<-ifelse(base_dades$farinacis_adj<0, base_dades$racio_farinacis/base_dades$kcal_sum, base_dades$farinacis_adj)
base_dades$racio_farinacis <- NULL

## Llegums
lm_llegum <- lm(racio_llegum ~ kcal_sum, data=base_dades)
base_dades$llegum_adj<-lm_llegum$coefficients[1] + lm_llegum$coefficients[2]*mitjana_energia + lm_llegum$residuals
base_dades$llegum_adj<-ifelse(base_dades$llegum_adj<0, base_dades$racio_llegum/base_dades$kcal_sum, base_dades$llegum_adj)
base_dades$racio_llegum <- NULL

## Productes processats
lm_processat <- lm(racio_processat ~ kcal_sum, data=base_dades)
base_dades$processat_adj<-lm_processat$coefficients[1] + lm_processat$coefficients[2]*mitjana_energia + lm_processat$residuals
base_dades$processat_adj<-ifelse(base_dades$processat_adj<0, base_dades$racio_processat/base_dades$kcal_sum, base_dades$processat_adj)
base_dades$racio_processat <- NULL

## Ous
lm_ous <- lm(racio_ous ~ kcal_sum, data=base_dades)
base_dades$ous_adj<-lm_ous$coefficients[1] + lm_ous$coefficients[2]*mitjana_energia + lm_ous$residuals
base_dades$ous_adj<-ifelse(base_dades$ous_adj<0, base_dades$racio_ous/base_dades$kcal_sum, base_dades$ous_adj)
base_dades$racio_ous <- NULL

## Carn blanca
lm_cblanca <- lm(racio_cblanca ~ kcal_sum, data=base_dades)
base_dades$cblanca_adj<-lm_cblanca$coefficients[1] + lm_cblanca$coefficients[2]*mitjana_energia + lm_cblanca$residuals
base_dades$cblanca_adj<-ifelse(base_dades$cblanca_adj<0, base_dades$racio_cblanca/base_dades$kcal_sum, base_dades$cblanca_adj)
base_dades$racio_cblanca <- NULL

## Carn vermella
lm_cvermella <- lm(racio_cvermella ~ kcal_sum, data=base_dades)
base_dades$cvermella_adj<-lm_cvermella$coefficients[1] + lm_cvermella$coefficients[2]*mitjana_energia + lm_cvermella$residuals
base_dades$cvermella_adj<-ifelse(base_dades$cvermella_adj<0, base_dades$racio_cvermella/base_dades$kcal_sum, base_dades$cvermella_adj)
base_dades$racio_cvermella <- NULL

## Carn processada
lm_cprocess <- lm(racio_cprocess ~ kcal_sum, data=base_dades)
base_dades$cprocess_adj<-lm_cprocess$coefficients[1] + lm_cprocess$coefficients[2]*mitjana_energia + lm_cprocess$residuals
base_dades$cprocess_adj<-ifelse(base_dades$cprocess_adj<0, base_dades$racio_cprocess/base_dades$kcal_sum, base_dades$cprocess_adj)
base_dades$racio_cprocess <- NULL

## Peix blanc
lm_pblanc <- lm(racio_pblanc ~ kcal_sum, data=base_dades)
base_dades$pblanc_adj<-lm_pblanc$coefficients[1] + lm_pblanc$coefficients[2]*mitjana_energia + lm_pblanc$residuals
base_dades$pblanc_adj<-ifelse(base_dades$pblanc_adj<0, base_dades$racio_pblanc/base_dades$kcal_sum, base_dades$pblanc_adj)
base_dades$racio_pblanc <- NULL

## Peix blau
lm_pblau <- lm(racio_pblau ~ kcal_sum, data=base_dades)
base_dades$pblau_adj<-lm_pblau$coefficients[1] + lm_pblau$coefficients[2]*mitjana_energia + lm_pblau$residuals
base_dades$pblau_adj<-ifelse(base_dades$pblau_adj<0, base_dades$racio_pblau/base_dades$kcal_sum, base_dades$pblau_adj)
base_dades$racio_pblau <- NULL

## Marisc
lm_pmarisc <- lm(racio_pmarisc ~ kcal_sum, data=base_dades)
base_dades$pmarisc_adj<-lm_pmarisc$coefficients[1] + lm_pmarisc$coefficients[2]*mitjana_energia + lm_pmarisc$residuals
base_dades$pmarisc_adj<-ifelse(base_dades$pmarisc_adj<0, base_dades$racio_pmarisc/base_dades$kcal_sum, base_dades$pmarisc_adj)
base_dades$racio_pmarisc <- NULL

## Fruita
lm_fruita <- lm(racio_fruita ~ kcal_sum, data=base_dades)
base_dades$fruita_adj<-lm_fruita$coefficients[1] + lm_fruita$coefficients[2]*mitjana_energia + lm_fruita$residuals
base_dades$fruita_adj<-ifelse(base_dades$fruita_adj<0, base_dades$racio_fruita/base_dades$kcal_sum, base_dades$fruita_adj)
base_dades$racio_fruita <- NULL

## Fruits secs
lm_fsecs <- lm(racio_fsecs ~ kcal_sum, data=base_dades)
base_dades$fsecs_adj<-lm_fsecs$coefficients[1] + lm_fsecs$coefficients[2]*mitjana_energia + lm_fsecs$residuals
base_dades$fsecs_adj<-ifelse(base_dades$fsecs_adj<0, base_dades$racio_fsecs/base_dades$kcal_sum, base_dades$fsecs_adj)
base_dades$racio_fsecs <- NULL

## Begudes no alcohòliques
lm_begNOalc <- lm(racio_begNOalc ~ kcal_sum, data=base_dades)
base_dades$begNOalc_adj<-lm_begNOalc$coefficients[1] + lm_begNOalc$coefficients[2]*mitjana_energia + lm_begNOalc$residuals
base_dades$begNOalc_adj<-ifelse(base_dades$begNOalc_adj<0, base_dades$racio_begNOalc/base_dades$kcal_sum, base_dades$begNOalc_adj)
base_dades$racio_begNOalc <- NULL

## Cervesa
lm_cervesa <- lm(Cervesa_set_V1grd ~ kcal_sum, data=base_dades)
base_dades$cervesa_adj<-lm_cervesa$coefficients[1] + lm_cervesa$coefficients[2]*mitjana_energia + lm_cervesa$residuals
base_dades$cervesa_adj<-ifelse(base_dades$cervesa_adj<0, base_dades$Cervesa_set_V1grd/base_dades$kcal_sum, base_dades$cervesa_adj)
base_dades$Cervesa_set_V1grd <- NULL

## Vi
lm_vi <- lm(Vi_set_V1grd ~ kcal_sum, data=base_dades)
base_dades$vi_adj<-lm_vi$coefficients[1] + lm_vi$coefficients[2]*mitjana_energia + lm_vi$residuals
base_dades$vi_adj<-ifelse(base_dades$vi_adj<0, base_dades$Vi_set_V1grd/base_dades$kcal_sum, base_dades$vi_adj)
base_dades$Vi_set_V1grd <- NULL

## Alcohols destil.lats
lm_destilades <- lm(Destilades_set_V1grd ~ kcal_sum, data=base_dades)
base_dades$destilades_adj<-lm_destilades$coefficients[1] + lm_destilades$coefficients[2]*mitjana_energia + lm_destilades$residuals
base_dades$destilades_adj<- ifelse(base_dades$destilades_adj<0, base_dades$Destilades_set_V1grd/base_dades$kcal_sum, base_dades$destilades_adj)
base_dades$Destilades_set_V1grd <- NULL

base_dades$kcal_sum <- NULL
