
# ANÀLISI DE COMPONENTS PRINCIPALS


###### LLIBRERIES ######

library(stats)
library(dplyr)
library(ggplot2)
library(reshape2)
library(labelled)



###### DADES ######

base_dades <- readRDS("ruta_arxiu")

dades_aliments <- base_dades %>%
  select(lactics_adj:destilades_adj)
  
rownames(dades_aliments) <- base_dades$COD_FAQCIL

dades_est <- scale(dades_aliments, center = TRUE, scale = TRUE)

noms_variables <- c("Prod_làctics", "Brioixeria_dolços", "Verdura", "Farinacis", "Llegums", "Prod_processats", "Ous", "Carn_blanca", "Carn_vermella", "Carn_processada", "Peix_blanc", "Peix_blau", "Marisc", "Fruita", "Fruits_secs", "Begudes_no_alc", "Cervesa", "Vi", "Alcohols_destil.lats")



###### ACP ######

# Apliquem l'ACP (center/scale=FALSE perquè ja ho hem fet abans)
pca <- prcomp(dades_est, center = FALSE, scale. = FALSE)

# Calculem els valors propis i ens quedem amb els que són >1,5 (3 primers components)
vaps<-pca$sdev^2

# Veiem la variança explicada per cada component
summary(pca)

# Extraiem les càrregues factorials fent producte dels vectors propis per les desviacions estàndard (que són l'arrel quadrada dels valors propis)
carregues_originals <- pca$rotation[, 1:3] %*% diag(pca$sdev[1:3]) 

# Fem rotació Varimax de les càrregues originals
rotacio <- varimax(carregues_originals)

# Noves càrregues rotades
carregues_rotades <- as.data.frame(unclass(rotacio$loadings))

# Matriu de les noves càrregues rotades
matriu_carregues_rotades <- as.matrix(rotacio$loadings)

# Puntuacions rotades (projectant dades estandaritzades en el nou espai de components)
puntuacions_rotades <- dades_est %*% matriu_carregues_rotades



###### TAULA CÀRREGUES FACTORIALS ######

matriu_carregues_rotades_v2 <- as.data.frame(lapply(carregues_rotades, function(x) {
  if(is.numeric(x)) round(x,3) else x
}))
matriu_carregues_rotades_v2[abs(matriu_carregues_rotades_v2) < 0.2] <- ""
rownames(matriu_carregues_rotades_v2) <- noms_variables
matriu_carregues_rotades_v2 <- labelled::set_variable_labels(matriu_carregues_rotades_v2, V1="Patró_1", V2="Patró_2", V3="Patró_3")
labels <- sapply(matriu_carregues_rotades_v2, function(x) attr(x, "label"))
colnames(matriu_carregues_rotades_v2)<-labels



###### GRÀFIC CÀRREGUES FACTORIALS ######

rownames(carregues_rotades) <- noms_variables
carregues_rotades$Variable <- factor(noms_variables, levels = noms_variables)
carregues <- melt(carregues_rotades, id.vars = "Variable")

ggplot(carregues, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Càrregues_dels_3_patrons_principals",
       x = "Variables", y = "Càrregues") +
  scale_fill_discrete(name = "Patrons", labels = c("Patró_1", "Patró_2", "Patró_3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(linetype = "solid", colour = "black"))
