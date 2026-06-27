# Segmentation Clients E-Commerce — ACP + K-Means

**Module :** Méthodes Statistiques et Étude de Données (Analyse Multivariée)  
**Langage :** R | **Packages :** tidyverse, FactoMineR, factoextra, cluster, ggplot2

---

## Structure du Projet

```
├── segmentation_clients_ecommerce.Rmd   ← Rapport complet RMarkdown (15-25 pages)
├── segmentation_clients_script.R        ← Script R autonome (exécution directe)
├── Online Retail.xlsx                   ← Dataset UCI (à télécharger manuellement)
└── README.md
```

---

## Instructions de Démarrage

### 1. Télécharger le Dataset

Rendez-vous sur le dépôt UCI :  
**https://archive.ics.uci.edu/ml/datasets/online+retail**

Téléchargez `Online Retail.xlsx` et placez-le dans le même répertoire que les fichiers R.

### 2. Installer les Packages R

```r
install.packages(c("tidyverse", "lubridate", "FactoMineR", "factoextra",
                   "cluster", "corrplot", "scales", "gridExtra",
                   "readxl", "knitr", "rmarkdown"))
```

### 3a. Générer le Rapport RMarkdown

Dans RStudio, ouvrez `segmentation_clients_ecommerce.Rmd` et cliquez sur **Knit**,
ou exécutez dans la console R :

```r
rmarkdown::render("segmentation_clients_ecommerce.Rmd",
                  output_format = "html_document")
```

### 3b. Exécuter le Script Autonome

```r
source("segmentation_clients_script.R")
```

---

## Méthodes Utilisées

| Étape | Méthode | Package |
|-------|---------|---------|
| Nettoyage & agrégation | Pipeline tidyverse | dplyr, lubridate |
| Statistiques descriptives | Tableau + visualisations | ggplot2, corrplot |
| Réduction dimensionnelle | ACP (Analyse en Composantes Principales) | FactoMineR, factoextra |
| Sélection de k | Elbow + Silhouette + Gap Stat | factoextra, cluster |
| Segmentation | K-Means (k=4, nstart=25) | stats, factoextra |
| Validation | Indice de Silhouette | cluster |
| Visualisation finale | Biplot ACP + Clusters, Heatmap, Radar | ggplot2, factoextra |

---

## Variables Comportementales Créées (8 variables)

| Variable | Description |
|----------|-------------|
| `TotalSpent` | Montant total dépensé (£) |
| `NumberInvoices` | Nombre de commandes distinctes |
| `TotalQuantity` | Quantité totale d'articles achetés |
| `AverageBasket` | Panier moyen par commande (£) |
| `Frequency` | Intervalle moyen entre achats (jours) |
| `Recency` | Jours depuis le dernier achat |
| `AveragePrice` | Prix unitaire moyen payé (£) |
| `NumberProducts` | Nombre de produits distincts achetés |

---

## Profils Clients Identifiés

| Segment | Profil | Stratégie |
|---------|--------|-----------|
| Cluster 1 | 🏆 VIP / Haute Valeur | Programme fidélité premium, accès exclusifs |
| Cluster 2 | 🔄 Fidèles Réguliers | Cross-selling, points de fidélité |
| Cluster 3 | 💤 Inactifs / À Risque | Campagne réactivation, offre urgence |
| Cluster 4 | 🆕 Nouveaux / Occasionnels | Onboarding, remise découverte |
