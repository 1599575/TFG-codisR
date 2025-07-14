
# ANÀLISI DE CLÚSTERS

set.seed(123)


###### LLIBRERIES ######

library(factoextra)
library(ggplot2)
library(cluster)
library(dplyr)
library(doParallel)
library(clue)



###### DADES ######

base_dades <- readRDS("ruta_arxiu")

dades_aliments <- base_dades %>%
  select(lactics_adj:destilades_adj)
  
noms_variables <- c("Prod_làctics", "Brioixeria_dolços", "Verdura", "Farinacis", "Llegums", "Prod_processats", "Ous", "Carn_blanca", "Carn_vermella", "Carn_processada", "Peix_blanc", "Peix_blau", "Marisc", "Fruita", "Fruits_secs", "Begudes_no_alc", "Cervesa", "Vi", "Alcohols_destil.lats")



###### FUNCIONS ######

# FUNCIONS PER A VALIDACIÓ

# Funció per generar dades pertorbades afegint soroll
generar_dades_pertorbades <- function(dades) {
  return(dades + matrix(rnorm(nrow(dades) * ncol(dades), mean = 0, sd = 0.2), nrow = nrow(dades)))
}

# Funció per calcular MMD 
minimum_matching_distance <- function(cluster1, cluster2) {
  matriu_confusio <- table(cluster1, cluster2) 
  # Algorisme hongarès:
  assignacio <- solve_LSAP(matriu_confusio, maximum = TRUE) 
  return(1 - sum(matriu_confusio[cbind(seq_along(assignacio), assignacio)]) / sum(matriu_confusio))
}

# Funció per calcular l'inestabilitat amb MMD
calcul_inestabilitat <- function(dades, k, B = 20) {
  particions <- foreach(i = 1:B, .combine = c) %do% {
    dades_pertorbades <- generar_dades_pertorbades(dades)
    clustering <- hclust(dist(dades_pertorbades), method = "ward.D2")
    list(cutree(clustering, k = k))
  }
  valors_mmd <- numeric(0)
  for (i in 1:(B-1)) {
    for (j in (i+1):B) {
      observacions_comunes <- intersect(names(particions[[i]]), names(particions[[j]]))
      if (length(observacions_comunes) >= k) {
        mmd <- minimum_matching_distance(
          particions[[i]][observacions_comunes], 
          particions[[j]][observacions_comunes]
        )
        valors_mmd <- c(valors_mmd, mmd)
      }
    }
  }
  return(mean(valors_mmd, na.rm = TRUE))
}


# FUNCIÓ PER FER EL CLUSTERING JERÀRQUIC

analisi_clusters_jerarquics <- function(dades, titol = "Anàlisi_de_Clústers", B = 20) {
  # Descorrelació de les dades
  S <- cov(dades)
  eig <- eigen(S)
  U <- eig$vectors
  lambda <- eig$values
  lambda_inv_sqrt <- diag(1/sqrt(lambda))
  S_inv_sqrt <- U %*% lambda_inv_sqrt %*% t(U)
  X <- scale(dades, center = TRUE, scale = FALSE)
  Z <- as.matrix(X) %*% S_inv_sqrt
  rownames(Z) <- paste0("obs_", 1:nrow(Z))
  
  # Clustering jeràrquic
  dist_Z <- dist(Z, method = "euclidean")
  hc_Z <- hclust(dist_Z, method = "ward.D2")
  
  # Visualització del dendrograma
  plot(hc_Z, labels = FALSE, main = " ", sub = " ", xlab = "Observacions", ylab = "Distància_d'agregació")
  
  # Càlcul inestabilitat
  ks <- 2:10
  inestabilitats <- sapply(ks, function(k) {
    calcul_inestabilitat(Z, k, B = B)
  })
  
  # Gràfic d'inestabilitat
  plot(ks, inestabilitats, type = "b", pch = 19, col = "blue",
       xlab = "Nombre_de_clústers_(k)", ylab = "Inestabilitat_mitjana")
  abline(v = ks[which.min(inestabilitats)], col = "red", lty = 2)
  
  # Tria del millor k i creació dels clústers
  k_optim <- ks[which.min(inestabilitats)]
  clust <- cutree(hc_Z, k = k_optim)
  
  # Afegir resultats a les dades
  dades_clusters <- as.data.frame(Z)
  dades_clusters$cluster <- clust
  
  # Resultats importants
  return(list(
    dades_descorrelacionades = Z,
    dades_amb_clusters = dades_clusters,
    model_jerarquic = hc_Z,
    inestabilitats = inestabilitats,
    k_optim = k_optim,
    assignacio_clusters = clust,
    taula = table(clust),
    titol = titol
  ))
}


# FUNCIÓ PER VISUALITZAR MITJANES DE CADA CLUSTER

visualitzacio_mitjanes <- function(resultats, noms_variables, metode = "jerarquic") {
  # Preparació de dades segons el mètode
  if (metode == "jerarquic") {
    dades <- resultats$dades_amb_clusters
    prefix <- "Clúster_"
  } else if (metode == "kmeans") {
    dades <- as.data.frame(resultats$dades_descorrelacionades)
    dades$cluster <- resultats$clusters_kmeans
    prefix <- "Clúster_"
  }
  
  # Calculem mitjanes per cluster
  mitjanes_cluster <- dades %>%
    group_by(cluster) %>%
    summarise(across(everything(), mean)) %>%
    pivot_longer(-cluster, names_to = "Variable", values_to = "Mean") %>%
    pivot_wider(names_from = cluster, values_from = Mean, names_prefix = prefix)
  
  # Canviem noms a les variables
  mitjanes_cluster$Variable <- factor(noms_variables, levels = noms_variables)
  
  # Preparem dades pel gràfic
  mitjanes_clust <- mitjanes_cluster %>%
    pivot_longer(starts_with(prefix), names_to = "cluster", values_to = "score")
  
  # Crear gràfic
  ggplot(mitjanes_clust, aes(x = Variable, y = score, col = cluster, group = cluster)) +
    geom_point() + geom_line() +
    xlab("") + ylab("Consum_estandaritzat") +
    scale_color_discrete(name = "Clúster") +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
          legend.position = "bottom", 
          legend.direction = "horizontal",
          legend.background = element_rect(linewidth = 0.5, linetype = "solid", colour = "black"))
}



###### ANÀLISI INICIAL ######

resultats_inicials <- analisi_clusters_jerarquics(dades_aliments, "Anàlisi_jeràrquic_inicial")

grafic_inicial <- visualitzacio_mitjanes(resultats_inicials, noms_variables)



###### ELIMINACIÓ D'OUTLIERS I SEGON ANÀLISI ######

cluster_petit <- names(which.min(table( resultats_inicials$assignacio_clusters)))

dades_filtrades <- dades_aliments[resultats_inicials$assignacio_clusters != cluster_petit, ]

resultats_filtrats <- analisi_clusters_jerarquics(dades_filtrades, "Anàlisi_jeràrquic_filtrat")

grafic_filtrat <- visualitzacio_mitjanes(resultats_filtrats, noms_variables)



###### ANÀLISI FINAL AMB K-MEANS ######

# Obtenim els centroides dels clústers jeràrquics
centroides <- resultats_filtrats$dades_amb_clusters %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean)) %>%
  select(-cluster) %>%
  as.matrix()

# Apliquem k-means partint dels centroides
resultats_kmeans <- kmeans(resultats_filtrats$dades_descorrelacionades, centers = centroides, iter.max = 100)

# Comparació amb el clustering previ
table(Jeràrquic = resultats_filtrats$assignacio_clusters, Kmeans = resultats_kmeans$cluster)

# Preparació dades
resultats_kmeans_complet <- list(
  dades_descorrelacionades = resultats_filtrats$dades_descorrelacionades,
  clusters_kmeans = resultats_kmeans$cluster,
  titol = "K-means"
)

# Visualització mitjanes
grafic_kmeans <- visualitzacio_mitjanes(resultats_kmeans_complet, noms_variables, metode = "kmeans")
