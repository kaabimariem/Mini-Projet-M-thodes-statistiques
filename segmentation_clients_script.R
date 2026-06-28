# ==============================================================================
# SEGMENTATION CLIENTS E-COMMERCE — ACP + K-MEANS
# Dataset : Online Retail (UCI Machine Learning Repository)
# Module  : Méthodes Statistiques et Étude de Données (Analyse Multivariée)
# ==============================================================================

# ── 0. PACKAGES ───────────────────────────────────────────────────────────────
pkgs <- c("tidyverse","lubridate","FactoMineR","factoextra",
          "cluster","corrplot","scales","gridExtra",
          "readxl","readr","purrr","stringr","mclust")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

library(readxl)   # read_excel()
library(readr)    # read_csv()
library(purrr)    # map_dbl()
library(stringr)  # str_trim(), str_starts()

# ── Palette (adaptative selon k) ──────────────────────────────────────────────
PALETTE <- c("#E53935","#43A047","#1E88E5","#FB8C00",
             "#8E24AA","#00ACC1","#F4511E","#6D4C41","#546E7A","#039BE5")

# ── 1. CHARGEMENT DES DONNÉES ─────────────────────────────────────────────────
# Priorité : Excel local → CSV local → téléchargement UCI

if (file.exists("Online Retail.xlsx")) {
  retail_raw <- read_excel("Online Retail.xlsx")
  message("✅ Fichier Excel local chargé : ", nrow(retail_raw), " lignes")
  
} else if (file.exists("Online Retail.csv")) {
  retail_raw <- read_csv("Online Retail.csv", show_col_types = FALSE)
  message("✅ Fichier CSV local chargé : ", nrow(retail_raw), " lignes")
  
} else {
  # read_excel() ne supporte pas les URLs → téléchargement préalable requis
  url_data  <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"
  dest_file <- tempfile(fileext = ".xlsx")
  
  message("⏳ Téléchargement du fichier depuis UCI…")
  retail_raw <- tryCatch({
    download.file(url_data, destfile = dest_file, mode = "wb", quiet = TRUE)
    df <- read_excel(dest_file)
    message("✅ Données chargées depuis UCI : ", nrow(df), " lignes")
    df
  }, error = function(e) {
    stop(paste0(
      "❌ Téléchargement échoué. Télécharge manuellement le fichier depuis :\n",
      "   https://archive.ics.uci.edu/ml/datasets/online+retail\n",
      "   et place 'Online Retail.xlsx' dans : ", getwd()
    ))
  })
}

cat("Dimensions brutes :", nrow(retail_raw), "×", ncol(retail_raw), "\n")

# ── 2. NETTOYAGE ──────────────────────────────────────────────────────────────
retail <- retail_raw %>%
  rename_with(str_trim) %>%
  mutate(
    InvoiceDate = as.POSIXct(InvoiceDate, format = "%d/%m/%Y %H:%M", tz = "UTC"),
    
    CustomerID  = as.character(CustomerID),
    Quantity    = as.numeric(Quantity),
    UnitPrice   = as.numeric(UnitPrice)
  ) %>%
  filter(
    !is.na(CustomerID),              # Supprimer CustomerID manquants
    !str_starts(InvoiceNo, "C"),     # Supprimer annulations
    Quantity  > 0,                   # Quantités positives
    UnitPrice > 0                    # Prix positifs
  ) %>%
  mutate(LineRevenue = Quantity * UnitPrice)

cat("Après nettoyage :", nrow(retail), "lignes\n")

# ── 3. DATE DE RÉFÉRENCE POUR LA RÉCENCE ──────────────────────────────────────
date_ref <- max(retail$InvoiceDate, na.rm = TRUE) + days(1)
cat("Date de référence :", format(date_ref, "%d/%m/%Y"), "\n")

# ── 4. AGRÉGATION PAR CLIENT ──────────────────────────────────────────────────
customer_df <- retail %>%
  group_by(CustomerID) %>%
  summarise(
    TotalSpent     = sum(LineRevenue, na.rm = TRUE),
    NumberInvoices = n_distinct(InvoiceNo),
    TotalQuantity  = sum(Quantity, na.rm = TRUE),
    AverageBasket  = TotalSpent / NumberInvoices,
    Frequency      = as.numeric(difftime(max(InvoiceDate), min(InvoiceDate),
                                         units = "days")) /
      pmax(NumberInvoices - 1, 1),
    Recency        = as.numeric(difftime(date_ref, max(InvoiceDate),
                                         units = "days")),
    AveragePrice   = mean(UnitPrice, na.rm = TRUE),
    NumberProducts = n_distinct(StockCode),
    .groups = "drop"
  ) %>%
  filter(
    TotalSpent <= quantile(TotalSpent, 0.99, na.rm = TRUE),
    TotalSpent  > 0,
    !is.na(Recency),
    !is.na(Frequency),
    is.finite(Frequency),
    is.finite(AverageBasket)
  )

vars_num <- c("TotalSpent","NumberInvoices","TotalQuantity","AverageBasket",
              "Frequency","Recency","AveragePrice","NumberProducts")

cat("Dataset client final :", nrow(customer_df), "clients ×",
    length(vars_num), "variables\n")

# ── Garde-fou : arrêt immédiat si dataset vide ────────────────────────────────
if (nrow(customer_df) == 0)
  stop("❌ customer_df est vide. Relancez TOUT le script depuis le début avec Ctrl+Shift+S")

# ── 5. STATISTIQUES DESCRIPTIVES + VISUALISATIONS DES DISTRIBUTIONS ──────────
print(summary(customer_df %>% select(all_of(vars_num))))

# Histogrammes des distributions de chaque variable (avant normalisation)
customer_df %>%
  select(all_of(vars_num)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valeur") %>%
  ggplot(aes(x = Valeur)) +
  geom_histogram(bins = 40, fill = "#1E88E5", color = "white", alpha = 0.85) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  labs(title = "Distribution des Variables — Avant Normalisation",
       x = "", y = "Fréquence") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

# Boxplots pour détecter les outliers par variable
customer_df %>%
  select(all_of(vars_num)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Valeur") %>%
  ggplot(aes(x = Variable, y = Valeur)) +
  geom_boxplot(fill = "#42A5F5", color = "#0D47A1",
               outlier.colour = "#D32F2F", outlier.size = 1, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +
  labs(title = "Boxplots des Variables — Détection des Outliers",
       x = "", y = "") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_blank(),
        strip.text  = element_text(face = "bold"))

# ── 6. NORMALISATION ──────────────────────────────────────────────────────────
# customer_scaled = dataframe purement numérique (JAMAIS de colonne Cluster dedans)

# Diagnostic avant normalisation
cat("Valeurs NA par variable avant normalisation :\n")
print(colSums(is.na(customer_df %>% select(all_of(vars_num)))))

cat("\nValeurs Inf par variable :\n")
print(sapply(customer_df %>% select(all_of(vars_num)),
             function(x) sum(is.infinite(x))))

# Nettoyage : remplacer Inf/-Inf par NA, puis supprimer les lignes NA restantes
customer_df <- customer_df %>%
  mutate(across(all_of(vars_num), ~ ifelse(is.infinite(.), NA, .))) %>%
  filter(if_all(all_of(vars_num), ~ !is.na(.)))

cat("\nClients après suppression NA/Inf :", nrow(customer_df), "\n")

# ── Garde-fou avant ACP ───────────────────────────────────────────────────────
if (nrow(customer_df) == 0)
  stop("❌ Aucun client valide après nettoyage. Relancez tout le script avec Ctrl+Shift+S")
if (nrow(customer_df) < 10)
  stop(paste("❌ Seulement", nrow(customer_df), "clients — dataset insuffisant pour l'ACP"))

# Normalisation Z-score
customer_scaled <- customer_df %>%
  select(all_of(vars_num)) %>%
  scale() %>%
  as.data.frame()
rownames(customer_scaled) <- customer_df$CustomerID

# Vérification finale : aucun NA dans customer_scaled
stopifnot("NA détectés dans customer_scaled après scale()" =
            sum(is.na(customer_scaled)) == 0)
cat("✅ Normalisation OK —", nrow(customer_scaled), "clients ×",
    ncol(customer_scaled), "variables, 0 NA\n")

# ── 6b. MATRICE DE CORRÉLATIONS ───────────────────────────────────────────────
# Visualiser les corrélations entre variables avant l'ACP
# permet de justifier la pertinence de la réduction de dimension

cor_matrix <- cor(customer_df %>% select(all_of(vars_num)), method = "pearson")
cat("\nMatrice de corrélations (Pearson) :\n")
print(round(cor_matrix, 2))

corrplot(cor_matrix,
         method   = "color",
         type     = "upper",
         order    = "hclust",
         addCoef.col = "black",
         number.cex  = 0.7,
         tl.col   = "black",
         tl.srt   = 45,
         col      = colorRampPalette(c("#1565C0","#FFFFFF","#C62828"))(200),
         title    = "Matrice de Corrélations — Variables Clients",
         mar      = c(0, 0, 2, 0))

# ── 7. ACP ────────────────────────────────────────────────────────────────────
pca_result  <- PCA(customer_scaled, scale.unit = FALSE, ncp = 8, graph = FALSE)
eigenvalues <- get_eigenvalue(pca_result)
print(round(eigenvalues, 3))

# Scree plot
p_scree <- fviz_eig(pca_result, addlabels = TRUE,
                    main = "Scree Plot — Variance Expliquée",
                    barfill = "#2196F3", barcolor = "#0D47A1")
print(p_scree)

# Cercle des corrélations
p_circle <- fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE,
                         gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
                         title = "Cercle des Corrélations")
print(p_circle)

# Contributions des variables aux axes PC1 et PC2
p_contrib_pc1 <- fviz_contrib(pca_result, choice = "var", axes = 1, top = 8,
                              fill = "#1E88E5", color = "#0D47A1") +
  labs(title = "Contributions des Variables — PC1") +
  theme_minimal()

p_contrib_pc2 <- fviz_contrib(pca_result, choice = "var", axes = 2, top = 8,
                              fill = "#43A047", color = "#1B5E20") +
  labs(title = "Contributions des Variables — PC2") +
  theme_minimal()

grid.arrange(p_contrib_pc1, p_contrib_pc2, ncol = 2)

# Contributions cumulées PC1+PC2
p_contrib_12 <- fviz_contrib(pca_result, choice = "var", axes = 1:2, top = 8,
                             fill = "#FB8C00", color = "#E65100") +
  labs(title = "Contributions Cumulées — PC1 + PC2") +
  theme_minimal()
print(p_contrib_12)

# ── 8. CHOIX DU NOMBRE DE CLUSTERS ───────────────────────────────────────────
set.seed(42)

# Méthode du coude — purrr::map_dbl() explicite
wss <- purrr::map_dbl(1:10, function(k) {
  kmeans(customer_scaled, centers = k, nstart = 25)$tot.withinss
})
p_elbow <- tibble(k = 1:10, WSS = wss) %>%
  ggplot(aes(k, WSS)) +
  geom_line(color = "#1565C0", linewidth = 1.2) +
  geom_point(size = 4, color = "#D32F2F") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Méthode du Coude", x = "k", y = "WSS") +
  theme_minimal()

# Silhouette — k optimal détecté automatiquement
p_sil <- fviz_nbclust(customer_scaled, kmeans,
                      method = "silhouette", k.max = 10) +
  labs(title = "Silhouette Moyenne") + theme_minimal()

grid.arrange(p_elbow, p_sil, ncol = 2)

# ── Choix automatique de k (coude = 1er genou + silhouette) ──────────────────
# Coude : k où la réduction de WSS devient < 10% de la réduction précédente
wss_diff  <- diff(wss)
wss_ratio <- abs(wss_diff[-1]) / abs(wss_diff[-length(wss_diff)])
k_elbow   <- which(wss_ratio < 0.15)[1] + 1       # +1 car diff décale d'un rang
k_elbow   <- if (is.na(k_elbow)) 4L else k_elbow  # fallback k=4

# Silhouette : k avec silhouette moyenne maximale
sil_scores <- purrr::map_dbl(2:10, function(k) {
  km  <- kmeans(customer_scaled, centers = k, nstart = 25, iter.max = 300)
  sil <- cluster::silhouette(km$cluster, dist(customer_scaled))
  mean(sil[, 3])
})
k_sil <- which.max(sil_scores) + 1  # +1 car on commence à k=2

# Décision finale : priorité à la silhouette, coude en tiebreak
k_optimal <- k_sil
cat(sprintf("k coude = %d | k silhouette = %d → k retenu = %d\n",
            k_elbow, k_sil, k_optimal))

# ── 9. K-MEANS (k optimal) ────────────────────────────────────────────────────
set.seed(42)
k         <- k_optimal
km_result <- kmeans(customer_scaled, centers = k, nstart = 25, iter.max = 200)

# Cluster ajouté UNIQUEMENT dans customer_df — pas dans customer_scaled
customer_df$Cluster <- factor(km_result$cluster,
                              labels = paste0("Cluster ", seq_len(k)))

cat("\nRépartition des clusters :\n")
print(table(customer_df$Cluster))

# ── 10. SILHOUETTE DU CLUSTERING ──────────────────────────────────────────────
# dist() sur customer_scaled SANS colonne Cluster (customer_scaled est purement numérique)
sil_obj <- silhouette(km_result$cluster, dist(customer_scaled))
cat("Silhouette moyenne :", round(mean(sil_obj[, 3]), 3), "\n")
print(fviz_silhouette(sil_obj, palette = "jco", ggtheme = theme_minimal()))

# ── 11. VISUALISATION CLUSTERS SUR PLAN FACTORIEL ────────────────────────────
ind_coords <- as.data.frame(pca_result$ind$coord[, 1:2]) %>%
  mutate(CustomerID = customer_df$CustomerID, Cluster = customer_df$Cluster)

centroids <- ind_coords %>%
  group_by(Cluster) %>%
  summarise(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2), .groups = "drop")

p_pca_clust <- ggplot(ind_coords, aes(Dim.1, Dim.2, color = Cluster)) +
  geom_point(size = 1.2, alpha = 0.4) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon",
               alpha = 0.08, type = "norm", level = 0.95) +
  geom_label(data = centroids, aes(label = Cluster, fill = Cluster),
             color = "white", fontface = "bold", size = 4, alpha = 0.85,
             show.legend = FALSE) +
  scale_color_manual(values = PALETTE[seq_len(k)]) +
  scale_fill_manual(values  = PALETTE[seq_len(k)]) +
  labs(title = "Segmentation Clients — ACP + K-Means",
       x = paste0("PC1 (", round(eigenvalues[1,2],1), "%)"),
       y = paste0("PC2 (", round(eigenvalues[2,2],1), "%)")) +
  theme_minimal(base_size = 12)

print(p_pca_clust)

# ── 12. PROFILS MOYENS PAR CLUSTER ───────────────────────────────────────────
cluster_profiles <- customer_df %>%
  group_by(Cluster) %>%
  summarise(N = n(),
            across(all_of(vars_num), ~ round(mean(.), 2)),
            .groups = "drop")

print(cluster_profiles)

# ── 13. HEATMAP DES Z-SCORES PAR CLUSTER ─────────────────────────────────────
heatmap_data <- customer_df %>%
  group_by(Cluster) %>%
  summarise(across(all_of(vars_num), mean), .groups = "drop") %>%
  mutate(across(all_of(vars_num), ~ (. - mean(.)) / sd(.))) %>%
  pivot_longer(all_of(vars_num), names_to = "Variable", values_to = "Z")

p_heat <- ggplot(heatmap_data, aes(Variable, Cluster, fill = Z)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Z, 2)), size = 3.5, fontface = "bold",
            color = ifelse(abs(heatmap_data$Z) > 0.8, "white", "black")) +
  scale_fill_gradient2(low = "#1565C0", mid = "#FAFAFA", high = "#C62828",
                       midpoint = 0) +
  labs(title = "Heatmap des Profils Clients par Segment",
       x = "", y = "", fill = "Z-score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

print(p_heat)

# ── 14. BOXPLOTS DES VARIABLES PAR CLUSTER ───────────────────────────────────
# Visualiser la distribution de chaque variable au sein de chaque segment
customer_df %>%
  select(all_of(vars_num), Cluster) %>%
  pivot_longer(all_of(vars_num), names_to = "Variable", values_to = "Valeur") %>%
  ggplot(aes(x = Cluster, y = Valeur, fill = Cluster)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.5, alpha = 0.75) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = PALETTE[seq_len(k)]) +
  labs(title = "Distribution des Variables par Cluster",
       x = "", y = "") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x  = element_text(angle = 30, hjust = 1),
        strip.text   = element_text(face = "bold"),
        legend.position = "none")

# ── 15. CLASSIFICATION ASCENDANTE HIÉRARCHIQUE (CAH) ─────────────────────────
# Comparaison avec K-Means — sur un échantillon pour la lisibilité du dendrogramme
set.seed(42)
n_cah    <- min(300, nrow(customer_scaled))   # max 300 clients pour le dendro
idx_cah  <- sample(seq_len(nrow(customer_scaled)), n_cah)
dist_cah <- dist(customer_scaled[idx_cah, ], method = "euclidean")
hc_res   <- hclust(dist_cah, method = "ward.D2")

# Dendrogramme
p_dendro <- fviz_dend(hc_res,
                      k            = k,
                      cex          = 0.4,
                      k_colors     = PALETTE[seq_len(k)],
                      color_labels_by_k = TRUE,
                      rect         = TRUE,
                      rect_fill    = TRUE,
                      rect_border  = PALETTE[seq_len(k)],
                      main         = paste0("Dendrogramme CAH (Ward.D2) — ",
                                            n_cah, " clients"),
                      sub          = paste("k =", k, "clusters"),
                      ylab         = "Hauteur")
print(p_dendro)

# Clusters CAH sur l'échantillon complet
# Note : dist() sur grande matrice peut être lent — utilisation de méthode "euclidean"
# ward.D2 = critère de Ward sur distances au carré (méthode recommandée)
set.seed(42)
hc_full      <- hclust(dist(customer_scaled, method = "euclidean"),
                       method = "ward.D2")
cah_clusters <- cutree(hc_full, k = k)

# Comparaison CAH vs K-Means (tableau de contingence)
cat("\n── Comparaison CAH vs K-Means ──\n")
comparison_table <- table(
  KMeans = customer_df$Cluster,
  CAH    = paste0("CAH-", cah_clusters)
)
print(comparison_table)

# Indice de Rand ajusté — mclust::adjustedRandIndex (cluster:: ne l'exporte pas)
rand_adj <- mclust::adjustedRandIndex(km_result$cluster, cah_clusters)
cat(sprintf("Indice de Rand Ajusté (K-Means vs CAH) : %.3f\n", rand_adj))
cat("  → (1 = accord parfait | 0 = accord aléatoire)\n")

# ── 16. INTERPRÉTATION MÉTIER DES CLUSTERS ───────────────────────────────────
# Attribution automatique de labels selon les profils moyens
profile_scaled <- customer_df %>%
  group_by(Cluster) %>%
  summarise(across(all_of(vars_num), mean), .groups = "drop")

# Label basé sur TotalSpent (rang) et Recency (rang inversé)
profile_scaled <- profile_scaled %>%
  mutate(
    rank_spent   = rank(-TotalSpent),
    rank_recency = rank(Recency),       # Recency faible = client récent
    rank_invoices= rank(-NumberInvoices),
    label = case_when(
      rank_spent   == 1 & rank_recency <= 2 ~ "💎 VIP / Fidèle Haut Valeur",
      rank_invoices== 1 & rank_recency <= 2 ~ "🔄 Acheteur Fréquent",
      rank_recency == max(rank_recency)     ~ "💤 Client Inactif / À Risque",
      rank_spent   == max(rank_spent)       ~ "🆕 Nouveau / Occasionnel",
      TRUE                                  ~ "📦 Client Régulier Moyen"
    )
  )

cat("\n── Interprétation Métier des Clusters ──\n")
print(profile_scaled %>% select(Cluster, TotalSpent, NumberInvoices,
                                Recency, AverageBasket, label))

cat("\n📌 Recommandations décisionnelles :\n")
cat("  💎 VIP         → Fidélisation premium, offres exclusives\n")
cat("  🔄 Fréquent    → Programme de récompenses, cross-selling\n")
cat("  💤 Inactif     → Campagnes de réactivation (email, promo)\n")
cat("  🆕 Nouveau     → Onboarding, première commande offerte\n")
cat("  📦 Régulier    → Upselling, amélioration du panier moyen\n")

message("\n✅ Analyse terminée — ", nrow(customer_df), " clients segmentés en ",
        k, " clusters (k optimal = ", k_optimal, ").")
