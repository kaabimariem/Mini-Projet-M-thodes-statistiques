# ==============================================================================
#  SEGMENTATION CLIENTS E-COMMERCE — APPLICATION SHINY
#  Dataset : Online Retail (UCI Machine Learning Repository)
#  Méthodes : Nettoyage → Agrégation → ACP (FactoMineR) → K-Means
# ==============================================================================

# ── 0. PACKAGES ───────────────────────────────────────────────────────────────
pkgs <- c("shiny","shinydashboard","tidyverse","lubridate",
          "FactoMineR","factoextra","cluster","ggplot2","DT","readxl")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

# ── Limite d'upload augmentée à 100 MB (Online Retail.xlsx ≈ 23 MB) ─────────
options(shiny.maxRequestSize = 100 * 1024^2)

# ── Palette de couleurs cohérente avec le projet ──────────────────────────────
PALETTE <- c("#E53935","#43A047","#1E88E5","#FB8C00",
             "#8E24AA","#00ACC1","#F4511E","#6D4C41","#546E7A","#039BE5")

VARS_NUM <- c("TotalSpent","NumberInvoices","TotalQuantity","AverageBasket",
              "Frequency","Recency","AveragePrice","NumberProducts")

# ==============================================================================
#  UI
# ==============================================================================
ui <- dashboardPage(
  skin = "blue",

  # ── En-tête ──────────────────────────────────────────────────────────────
  dashboardHeader(
    title = span(icon("store"), " Segmentation Clients E-Commerce"),
    titleWidth = 340
  ),

  # ── Sidebar ──────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sidebar",
      menuItem("Accueil",              tabName = "home",    icon = icon("house")),
      menuItem("Dataset",              tabName = "dataset", icon = icon("table")),
      menuItem("Statistiques",         tabName = "stats",   icon = icon("chart-bar")),
      menuItem("ACP",                  tabName = "pca",     icon = icon("circle-nodes")),
      menuItem("Clustering K-Means",   tabName = "kmeans",  icon = icon("object-group")),
      menuItem("Profils Clients",      tabName = "profils", icon = icon("users")),
      menuItem("Interprétation",       tabName = "interp",  icon = icon("lightbulb"))
    ),
    hr(),

    # ── Panneau de contrôle global ────────────────────────────────────────
    div(style = "padding: 10px 15px;",
      h5(icon("sliders"), " Paramètres", style = "color:#90CAF9; font-weight:bold;"),

      fileInput("file_upload",
                label    = "Importer le dataset",
                accept   = c(".xlsx", ".xls", ".csv"),
                buttonLabel = "Parcourir…",
                placeholder = "Online Retail.xlsx / .csv"),

      conditionalPanel(
        condition = "output.data_loaded",
        sliderInput("n_clusters",
                    label = "Nombre de clusters (k)",
                    min = 2, max = 10, value = 4, step = 1),
        actionButton("run_analysis",
                     label = span(icon("play"), " Lancer l'analyse"),
                     class = "btn-success btn-block",
                     style = "margin-top:8px; font-weight:bold;"),
        hr(),
        uiOutput("kpi_sidebar")
      )
    )
  ),

  # ── Corps ────────────────────────────────────────────────────────────────
  dashboardBody(

    # CSS personnalisé
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #F5F7FA; }
      .box { border-radius: 8px; }
      .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #1976D2; }
      .info-box { border-radius: 8px; }
      .sidebar-menu li a { font-size: 13px; }
      .shiny-notification { width: 340px; }
      .plot-container { min-height: 420px; }
    "))),

    tabItems(

      # ────────────────────────────────────────────────────────────────────
      # ONGLET 0 : ACCUEIL
      # ────────────────────────────────────────────────────────────────────
      tabItem(tabName = "home",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = span(icon("circle-info"), " Guide d'utilisation"),
              HTML("
                <ol style='font-size:14px; line-height:2;'>
                  <li><b>Importez</b> votre fichier <code>Online Retail.xlsx</code>
                      (ou <code>.csv</code>) via le panneau gauche.</li>
                  <li>Choisissez le <b>nombre de clusters</b> k (2–10) avec le slider.</li>
                  <li>Cliquez sur <b>Lancer l'analyse</b> pour déclencher :
                    nettoyage → agrégation → ACP → K-Means.</li>
                  <li>Naviguez dans les onglets pour explorer les résultats.</li>
                </ol>
                <hr/>
                <b>Variables comportementales créées :</b>
                TotalSpent · NumberInvoices · TotalQuantity · AverageBasket ·
                Frequency · Recency · AveragePrice · NumberProducts
              ")
          )
        ),
        fluidRow(
          infoBox("Méthode",    "ACP + K-Means",   icon = icon("diagram-project"), color = "blue",   width = 4),
          infoBox("Variables",  "8 comportementales", icon = icon("list-check"),   color = "green",  width = 4),
          infoBox("Dataset",    "Online Retail UCI",  icon = icon("database"),     color = "orange", width = 4)
        )
      ),

      # ────────────────────────────────────────────────────────────────────
      # ONGLET 1 : DATASET
      # ────────────────────────────────────────────────────────────────────
      tabItem(tabName = "dataset",
        fluidRow(
          valueBoxOutput("vbox_raw",      width = 3),
          valueBoxOutput("vbox_clean",    width = 3),
          valueBoxOutput("vbox_clients",  width = 3),
          valueBoxOutput("vbox_period",   width = 3)
        ),
        fluidRow(
          tabBox(width = 12, title = "Données",
            tabPanel(icon("database"), "Transactions brutes",
                     DTOutput("table_raw")),
            tabPanel(icon("user-check"), "Dataset client agrégé",
                     DTOutput("table_customer"))
          )
        )
      ),

      # ────────────────────────────────────────────────────────────────────
      # ONGLET 2 : STATISTIQUES DESCRIPTIVES
      # ────────────────────────────────────────────────────────────────────
      tabItem(tabName = "stats",
        fluidRow(
          box(width = 12, status = "info", solidHeader = TRUE,
              title = span(icon("table"), " Statistiques descriptives"),
              DTOutput("table_stats"))
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = span(icon("chart-area"), " Distributions"),
              plotOutput("plot_distributions", height = "500px")),
          box(width = 6, status = "warning", solidHeader = TRUE,
              title = span(icon("border-all"), " Matrice de corrélations"),
              plotOutput("plot_corr", height = "500px"))
        )
      ),

      # ────────────────────────────────────────────────────────────────────
      # ONGLET 3 : ACP
      # ────────────────────────────────────────────────────────────────────
      tabItem(tabName = "pca",
        fluidRow(
          box(width = 12, status = "info", solidHeader = TRUE,
              title = span(icon("table"), " Valeurs propres et variance expliquée"),
              DTOutput("table_eigenvalues"))
        ),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = span(icon("chart-bar"), " Scree Plot"),
              plotOutput("plot_scree", height = "400px")),
          box(width = 6, status = "success", solidHeader = TRUE,
              title = span(icon("circle-nodes"), " Cercle des corrélations"),
              plotOutput("plot_circle", height = "400px"))
        ),
        # ── ❸ Interprétation dynamique des axes ACP ──────────────────────
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = span(icon("comment-dots"), " Interprétation des axes factoriels"),
              uiOutput("ui_pca_interpretation"))
        ),
        fluidRow(
          box(width = 6, status = "warning", solidHeader = TRUE,
              title = span(icon("circle-dot"), " Projection des individus"),
              plotOutput("plot_ind", height = "420px")),
          box(width = 6, status = "danger", solidHeader = TRUE,
              title = span(icon("diagram-project"), " Biplot"),
              plotOutput("plot_biplot", height = "420px"))
        )
      ),

      # ────────────────────────────────────────────────────────────────────
      # ONGLET 4 : CLUSTERING
      # ────────────────────────────────────────────────────────────────────
      tabItem(tabName = "kmeans",
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
              title = span(icon("chart-line"), " Méthode du coude (Elbow)"),
              plotOutput("plot_elbow", height = "380px")),
          box(width = 6, status = "success", solidHeader = TRUE,
              title = span(icon("ruler"), " Silhouette moyenne"),
              plotOutput("plot_silhouette_nbclust", height = "380px"))
        ),
        fluidRow(
          box(width = 8, status = "warning", solidHeader = TRUE,
              title = span(icon("object-group"), " Clusters sur le plan factoriel ACP"),
              plotOutput("plot_clusters_pca", height = "480px")),
          box(width = 4, status = "danger", solidHeader = TRUE,
              title = span(icon("chart-pie"), " Diagramme de silhouette"),
              plotOutput("plot_sil_diagram", height = "480px"))
        )
      ),

      # ────────────────────────────────────────────────────────────────────
      # ONGLET 5 : PROFILS CLIENTS
      # ────────────────────────────────────────────────────────────────────
      tabItem(tabName = "profils",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
              title = span(icon("table"), " Profils moyens par cluster"),
              DTOutput("table_profiles"))
        ),
        fluidRow(
          box(width = 7, status = "warning", solidHeader = TRUE,
              title = span(icon("border-all"), " Heatmap des Z-scores par cluster"),
              plotOutput("plot_heatmap", height = "380px")),
          box(width = 5, status = "success", solidHeader = TRUE,
              title = span(icon("chart-pie"), " Répartition des clients"),
              plotOutput("plot_pie", height = "380px"))
        ),
        fluidRow(
          box(width = 12, status = "info", solidHeader = TRUE,
              title = span(icon("users"), " Dataset client avec clusters"),
              DTOutput("table_customer_clustered"))
        )
      ),

      # ────────────────────────────────────────────────────────────────────
      # ONGLET 6 : INTERPRÉTATION & RECOMMANDATIONS
      # ────────────────────────────────────────────────────────────────────
      tabItem(tabName = "interp",

        # ── ❶ Nommage dynamique des profils ──────────────────────────────
        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE,
              title = span(icon("tags"), " Nommage automatique des profils clients"),
              uiOutput("ui_profil_nommage"))
        ),

        # ── ❷ Recommandations marketing ──────────────────────────────────
        fluidRow(
          box(width = 12, status = "warning", solidHeader = TRUE,
              title = span(icon("bullhorn"), " Recommandations Marketing par Segment"),
              DTOutput("table_recommandations"))
        ),

        # ── Synthèse décisionnelle ────────────────────────────────────────
        fluidRow(
          box(width = 12, status = "danger", solidHeader = TRUE,
              title = span(icon("circle-check"), " Synthèse & Impact Décisionnel"),
              uiOutput("ui_synthese"))
        )
      )

    ) # fin tabItems
  )   # fin dashboardBody
)     # fin dashboardPage

# ==============================================================================
#  SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ── Navigation automatique après chargement ──────────────────────────────
  observeEvent(input$file_upload, {
    updateTabItems(session, "sidebar", "dataset")
  })

  # ────────────────────────────────────────────────────────────────────────────
  #  REACTIVE : Données brutes (lecture fichier)
  # ────────────────────────────────────────────────────────────────────────────
  raw_data <- reactive({
    req(input$file_upload)
    path <- input$file_upload$datapath
    ext  <- tools::file_ext(input$file_upload$name)

    withProgress(message = "Lecture du fichier…", value = 0.2, {
      df <- tryCatch({
        if (ext %in% c("xlsx","xls")) {
          readxl::read_excel(path)
        } else {
          readr::read_csv(path, show_col_types = FALSE)
        }
      }, error = function(e) {
        showNotification(paste("Erreur lecture :", e$message),
                         type = "error", duration = 8)
        return(NULL)
      })
    })
    df
  })

  # Indicateur de disponibilité des données pour le UI conditionnel
  output$data_loaded <- reactive({ !is.null(raw_data()) })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # ────────────────────────────────────────────────────────────────────────────
  #  REACTIVE : Données nettoyées + agrégation client
  #  Déclenché uniquement au clic sur "Lancer l'analyse"
  # ────────────────────────────────────────────────────────────────────────────
  analysis <- eventReactive(input$run_analysis, {

    req(raw_data())
    df_raw <- raw_data()

    withProgress(message = "Analyse en cours…", value = 0, {

      # ── Nettoyage ──────────────────────────────────────────────────────
      setProgress(0.1, detail = "Nettoyage des données…")
      retail <- df_raw %>%
        rename_with(str_trim) %>%
        mutate(
          InvoiceDate = lubridate::parse_date_time(
            InvoiceDate,
            orders = c("dmy HM","mdy HM","ymd HM","dmy HMS","mdy HMS",
                       "Y-m-d H:M:S","m/d/Y H:M")
          ),
          CustomerID = as.character(CustomerID),
          Quantity   = as.numeric(Quantity),
          UnitPrice  = as.numeric(UnitPrice)
        ) %>%
        filter(
          !is.na(CustomerID),
          !is.na(InvoiceDate),
          !str_starts(as.character(InvoiceNo), "C"),
          Quantity  > 0,
          UnitPrice > 0
        ) %>%
        mutate(LineRevenue = Quantity * UnitPrice)

      if (nrow(retail) == 0) {
        showNotification("Aucune ligne valide après nettoyage.", type = "error")
        return(NULL)
      }

      # ── Agrégation client ───────────────────────────────────────────────
      setProgress(0.25, detail = "Agrégation par client…")
      date_ref <- max(retail$InvoiceDate, na.rm = TRUE) + lubridate::days(1)

      customer_df <- retail %>%
        group_by(CustomerID) %>%
        summarise(
          TotalSpent     = sum(LineRevenue, na.rm = TRUE),
          NumberInvoices = n_distinct(InvoiceNo),
          TotalQuantity  = sum(Quantity, na.rm = TRUE),
          AverageBasket  = TotalSpent / NumberInvoices,
          Frequency      = as.numeric(
            difftime(max(InvoiceDate), min(InvoiceDate), units = "days")
          ) / pmax(NumberInvoices - 1, 1),
          Recency        = as.numeric(
            difftime(date_ref, max(InvoiceDate), units = "days")
          ),
          AveragePrice   = mean(UnitPrice, na.rm = TRUE),
          NumberProducts = n_distinct(StockCode),
          .groups = "drop"
        ) %>%
        filter(
          TotalSpent <= quantile(TotalSpent, 0.99, na.rm = TRUE),
          TotalSpent > 0
        )

      # ── Normalisation ───────────────────────────────────────────────────
      setProgress(0.40, detail = "Normalisation…")
      customer_scaled <- customer_df %>%
        select(all_of(VARS_NUM)) %>%
        scale() %>%
        as.data.frame()
      rownames(customer_scaled) <- customer_df$CustomerID

      # ── ACP ─────────────────────────────────────────────────────────────
      setProgress(0.55, detail = "ACP en cours…")
      pca_result  <- PCA(customer_scaled, scale.unit = FALSE,
                         ncp = 8, graph = FALSE)
      eigenvalues <- get_eigenvalue(pca_result)

      # ── K-Means ─────────────────────────────────────────────────────────
      k <- isolate(input$n_clusters)
      setProgress(0.75, detail = paste0("K-Means k=", k, "…"))
      set.seed(42)
      km_result <- kmeans(customer_scaled, centers = k,
                          nstart = 25, iter.max = 200)

      customer_df <- customer_df %>%
        mutate(Cluster = factor(km_result$cluster,
                                labels = paste0("Cluster ", seq_len(k))))

      # ── Silhouette ──────────────────────────────────────────────────────
      setProgress(0.90, detail = "Silhouette…")
      sil_obj <- silhouette(km_result$cluster, dist(customer_scaled))

      setProgress(1.0, detail = "Terminé ✅")
      showNotification(
        paste0("✅ ", nrow(customer_df), " clients segmentés en ", k, " clusters."),
        type = "message", duration = 5
      )

      list(
        retail          = retail,
        customer_df     = customer_df,
        customer_scaled = customer_scaled,
        date_ref        = date_ref,
        pca_result      = pca_result,
        eigenvalues     = eigenvalues,
        km_result       = km_result,
        sil_obj         = sil_obj,
        k               = k
      )
    })
  })

  # ────────────────────────────────────────────────────────────────────────────
  #  KPI SIDEBAR
  # ────────────────────────────────────────────────────────────────────────────
  output$kpi_sidebar <- renderUI({
    res <- analysis()
    req(res)
    sil_mean <- round(mean(res$sil_obj[, "sil_width"]), 3)
    div(
      tags$small(style = "color:#90CAF9;",
        icon("users"), strong(" Clients : "), nrow(res$customer_df), br(),
        icon("layer-group"), strong(" Clusters : "), res$k, br(),
        icon("ruler"), strong(" Silhouette : "), sil_mean
      )
    )
  })

  # ────────────────────────────────────────────────────────────────────────────
  #  ONGLET 1 — VALUE BOXES & TABLES
  # ────────────────────────────────────────────────────────────────────────────
  output$vbox_raw <- renderValueBox({
    req(raw_data())
    valueBox(format(nrow(raw_data()), big.mark = " "),
             "Transactions brutes", icon = icon("receipt"),
             color = "blue")
  })

  output$vbox_clean <- renderValueBox({
    res <- analysis()
    req(res)
    valueBox(format(nrow(res$retail), big.mark = " "),
             "Transactions valides", icon = icon("check-circle"),
             color = "green")
  })

  output$vbox_clients <- renderValueBox({
    res <- analysis()
    req(res)
    valueBox(format(nrow(res$customer_df), big.mark = " "),
             "Clients uniques", icon = icon("users"),
             color = "orange")
  })

  output$vbox_period <- renderValueBox({
    res <- analysis()
    req(res)
    date_min <- format(min(res$retail$InvoiceDate, na.rm = TRUE), "%m/%Y")
    date_max <- format(max(res$retail$InvoiceDate, na.rm = TRUE), "%m/%Y")
    valueBox(paste(date_min, "→", date_max),
             "Période couverte", icon = icon("calendar"),
             color = "purple")
  })

  output$table_raw <- renderDT({
    req(raw_data())
    datatable(raw_data(),
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = "lfrtip"),
              filter = "top", class = "table-striped table-hover")
  })

  output$table_customer <- renderDT({
    res <- analysis()
    req(res)
    datatable(res$customer_df %>% select(-Cluster) %>%
                mutate(across(where(is.numeric), ~ round(., 2))),
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top", class = "table-striped table-hover")
  })

  # ────────────────────────────────────────────────────────────────────────────
  #  ONGLET 2 — STATISTIQUES DESCRIPTIVES
  # ────────────────────────────────────────────────────────────────────────────
  output$table_stats <- renderDT({
    res <- analysis()
    req(res)
    stats <- res$customer_df %>%
      select(all_of(VARS_NUM)) %>%
      pivot_longer(everything(), names_to = "Variable") %>%
      group_by(Variable) %>%
      summarise(
        N          = n(),
        Moyenne    = round(mean(value), 2),
        Médiane    = round(median(value), 2),
        Écart_type = round(sd(value), 2),
        Min        = round(min(value), 2),
        Q1         = round(quantile(value, .25), 2),
        Q3         = round(quantile(value, .75), 2),
        Max        = round(max(value), 2),
        .groups = "drop"
      )
    datatable(stats, options = list(dom = "t", pageLength = 10),
              class = "table-striped table-hover") %>%
      formatStyle("Moyenne",    background = styleColorBar(c(0, max(stats$Moyenne)), "#BBDEFB")) %>%
      formatStyle("Écart_type", background = styleColorBar(c(0, max(stats$Écart_type)), "#FFE0B2"))
  })

  output$plot_distributions <- renderPlot({
    res <- analysis()
    req(res)
    res$customer_df %>%
      select(all_of(VARS_NUM)) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Valeur") %>%
      ggplot(aes(x = Valeur, fill = Variable)) +
      geom_histogram(aes(y = after_stat(density)), bins = 35,
                     color = "white", alpha = 0.75) +
      geom_density(color = "black", linewidth = 0.7) +
      facet_wrap(~ Variable, scales = "free", ncol = 2) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Distributions des Variables Comportementales",
           x = "", y = "Densité") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold"))
  })

  output$plot_corr <- renderPlot({
    res <- analysis()
    req(res)
    cor_mat <- cor(res$customer_df %>% select(all_of(VARS_NUM)),
                   use = "complete.obs")
    corrplot::corrplot(cor_mat,
             method      = "color", type = "upper", order = "hclust",
             addCoef.col = "black", number.cex = 0.72,
             tl.col = "black", tl.srt = 45,
             col = colorRampPalette(c("#1565C0","#FFFFBF","#C62828"))(200),
             title = "Corrélations de Pearson", mar = c(0,0,1.5,0))
  })

  # ────────────────────────────────────────────────────────────────────────────
  #  ONGLET 3 — ACP
  # ────────────────────────────────────────────────────────────────────────────
  output$table_eigenvalues <- renderDT({
    res <- analysis()
    req(res)
    df_eig <- round(res$eigenvalues, 3) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Composante") %>%
      rename(Valeur_propre   = eigenvalue,
             Variance_pct    = `variance.percent`,
             Variance_cum    = `cumulative.variance.percent`)
    datatable(df_eig,
              options = list(dom = "t", pageLength = 10),
              class = "table-striped table-hover") %>%
      formatStyle("Variance_cum",
                  background = styleColorBar(c(0, 100), "#C8E6C9"),
                  backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$plot_scree <- renderPlot({
    res <- analysis()
    req(res)
    fviz_eig(res$pca_result, addlabels = TRUE,
             barfill = "#2196F3", barcolor = "#0D47A1",
             linecolor = "#E53935",
             main = "Scree Plot — Variance Expliquée par Composante",
             xlab = "Composante Principale", ylab = "% Variance") +
      geom_hline(yintercept = 100 / length(VARS_NUM),
                 linetype = "dashed", color = "orange", linewidth = 1) +
      theme_minimal()
  })

  output$plot_circle <- renderPlot({
    res <- analysis()
    req(res)
    fviz_pca_var(res$pca_result,
                 col.var = "contrib", repel = TRUE,
                 gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
                 title = "Cercle des Corrélations") +
      theme_minimal()
  })

  output$plot_ind <- renderPlot({
    res <- analysis()
    req(res)
    fviz_pca_ind(res$pca_result,
                 col.ind = "cos2",
                 gradient.cols = c("#E3F2FD","#1565C0","#0D47A1"),
                 geom.ind = "point", pointsize = 1.2, alpha.ind = 0.45,
                 title = "Projection des Individus sur PC1 × PC2") +
      theme_minimal()
  })

  output$plot_biplot <- renderPlot({
    res <- analysis()
    req(res)
    fviz_pca_biplot(res$pca_result,
                    col.ind = "cos2", col.var = "#D32F2F",
                    gradient.cols = c("#E3F2FD","#1565C0"),
                    geom.ind = "point", pointsize = 0.8, alpha.ind = 0.4,
                    repel = TRUE, title = "Biplot ACP") +
      theme_minimal()
  })

  # ────────────────────────────────────────────────────────────────────────────
  #  ONGLET 4 — CLUSTERING
  # ────────────────────────────────────────────────────────────────────────────
  output$plot_elbow <- renderPlot({
    res <- analysis()
    req(res)
    # Recalcul WSS pour k = 1:10 (utilise les données déjà normalisées)
    set.seed(42)
    wss_vals <- purrr::map_dbl(1:10, function(k) {
      kmeans(res$customer_scaled, centers = k,
             nstart = 15, iter.max = 100)$tot.withinss
    })
    pal <- rep("#1565C0", 10)
    pal[res$k] <- "#D32F2F"

    tibble(k = 1:10, WSS = wss_vals) %>%
      ggplot(aes(k, WSS)) +
      geom_line(color = "#1565C0", linewidth = 1.3) +
      geom_point(size = 5, color = pal) +
      geom_point(data = tibble(k = res$k, WSS = wss_vals[res$k]),
                 aes(k, WSS), shape = 21, size = 9,
                 color = "#D32F2F", fill = "transparent", stroke = 2) +
      annotate("text", x = res$k + 0.4, y = wss_vals[res$k],
               label = paste0("k = ", res$k),
               color = "#D32F2F", fontface = "bold", size = 4.5) +
      scale_x_continuous(breaks = 1:10) +
      labs(title = "Méthode du Coude — Inertie Intra-Classe",
           x = "Nombre de clusters k", y = "WSS (Within Sum of Squares)") +
      theme_minimal(base_size = 13)
  })

  output$plot_silhouette_nbclust <- renderPlot({
    res <- analysis()
    req(res)
    set.seed(42)
    fviz_nbclust(res$customer_scaled, kmeans,
                 method = "silhouette", k.max = 10,
                 linecolor = "#43A047") +
      geom_vline(xintercept = res$k, linetype = "dashed",
                 color = "#D32F2F", linewidth = 1) +
      labs(title = "Silhouette Moyenne par k",
           x = "Nombre de clusters k", y = "Silhouette moyenne") +
      theme_minimal(base_size = 13)
  })

  output$plot_clusters_pca <- renderPlot({
    res <- analysis()
    req(res)
    k   <- res$k
    pal <- PALETTE[seq_len(k)]

    ind_coords <- as.data.frame(res$pca_result$ind$coord[, 1:2]) %>%
      mutate(Cluster = res$customer_df$Cluster)

    centroids <- ind_coords %>%
      group_by(Cluster) %>%
      summarise(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2), .groups = "drop")

    eig <- res$eigenvalues
    ggplot(ind_coords, aes(Dim.1, Dim.2, color = Cluster)) +
      geom_point(size = 1.4, alpha = 0.40) +
      stat_ellipse(aes(fill = Cluster), geom = "polygon",
                   alpha = 0.08, type = "norm", level = 0.95) +
      geom_label(data = centroids, aes(label = Cluster, fill = Cluster),
                 color = "white", fontface = "bold", size = 4,
                 alpha = 0.88, show.legend = FALSE) +
      scale_color_manual(values = pal) +
      scale_fill_manual(values  = pal) +
      labs(title = paste0("Clusters K-Means (k=", k, ") sur Plan Factoriel ACP"),
           subtitle = "Ellipses de confiance à 95% — Étiquettes = centroïdes",
           x = paste0("PC1 (", round(eig[1,2],1), "% variance)"),
           y = paste0("PC2 (", round(eig[2,2],1), "% variance)"),
           color = "Cluster", fill = "Cluster") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$plot_sil_diagram <- renderPlot({
    res <- analysis()
    req(res)
    fviz_silhouette(res$sil_obj,
                    palette   = PALETTE[seq_len(res$k)],
                    ggtheme   = theme_minimal(base_size = 12),
                    print.summary = FALSE) +
      labs(title = "Diagramme de Silhouette",
           subtitle = paste0("Moyenne : ",
                             round(mean(res$sil_obj[,"sil_width"]), 3)))
  })

  # ────────────────────────────────────────────────────────────────────────────
  #  ONGLET 5 — PROFILS CLIENTS
  # ────────────────────────────────────────────────────────────────────────────
  output$table_profiles <- renderDT({
    res <- analysis()
    req(res)
    profiles <- res$customer_df %>%
      group_by(Cluster) %>%
      summarise(
        N              = n(),
        TotalSpent_moy = round(mean(TotalSpent), 0),
        NbFactures_moy = round(mean(NumberInvoices), 1),
        TotalQte_moy   = round(mean(TotalQuantity), 0),
        PanierMoy      = round(mean(AverageBasket), 0),
        Frequence_moy  = round(mean(Frequency), 1),
        Recence_moy    = round(mean(Recency), 0),
        PrixMoy        = round(mean(AveragePrice), 2),
        NbProduits_moy = round(mean(NumberProducts), 0),
        .groups = "drop"
      )
    datatable(profiles,
              options = list(dom = "t", pageLength = 15, scrollX = TRUE),
              class = "table-striped table-hover table-bordered") %>%
      formatStyle("TotalSpent_moy",
                  background = styleColorBar(c(0, max(profiles$TotalSpent_moy)),
                                             "#BBDEFB")) %>%
      formatStyle("Recence_moy",
                  background = styleColorBar(c(0, max(profiles$Recence_moy)),
                                             "#FFCCBC"))
  })

  output$plot_heatmap <- renderPlot({
    res <- analysis()
    req(res)
    hm <- res$customer_df %>%
      group_by(Cluster) %>%
      summarise(across(all_of(VARS_NUM), mean), .groups = "drop") %>%
      mutate(across(all_of(VARS_NUM), ~ (. - mean(.)) / (sd(.) + 1e-9))) %>%
      pivot_longer(all_of(VARS_NUM), names_to = "Variable", values_to = "Z")

    ggplot(hm, aes(x = Variable, y = Cluster, fill = Z)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = round(Z, 2)),
                size = 3.8, fontface = "bold",
                color = ifelse(abs(hm$Z) > 0.7, "white", "grey20")) +
      scale_fill_gradient2(low = "#1565C0", mid = "#FAFAFA", high = "#C62828",
                           midpoint = 0, name = "Z-score") +
      scale_x_discrete(guide = guide_axis(angle = 35)) +
      labs(title = "Heatmap des Profils Clients — Z-scores par Cluster",
           subtitle = "Rouge = au-dessus de la moyenne | Bleu = en dessous",
           x = "", y = "") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(face = "bold"))
  })

  output$plot_pie <- renderPlot({
    res <- analysis()
    req(res)
    k   <- res$k
    pal <- PALETTE[seq_len(k)]

    repartition <- res$customer_df %>%
      count(Cluster) %>%
      mutate(
        Pct   = round(n / sum(n) * 100, 1),
        Label = paste0(Cluster, "\n", n, " (", Pct, "%)")
      )

    ggplot(repartition, aes(x = "", y = n, fill = Cluster)) +
      geom_col(width = 1, color = "white", linewidth = 0.8) +
      coord_polar(theta = "y") +
      geom_text(aes(label = Label),
                position = position_stack(vjust = 0.5),
                size = 3.8, fontface = "bold", color = "white") +
      scale_fill_manual(values = pal) +
      labs(title = "Répartition des Clients par Cluster",
           fill = "Cluster") +
      theme_void(base_size = 12) +
      theme(legend.position = "right")
  })

  output$table_customer_clustered <- renderDT({
    res <- analysis()
    req(res)
    datatable(
      res$customer_df %>%
        mutate(across(where(is.numeric), ~ round(., 2))),
      filter  = "top",
      options = list(pageLength = 10, scrollX = TRUE,
                     dom = "lfrtip"),
      class   = "table-striped table-hover"
    ) %>%
      formatStyle("Cluster",
                  backgroundColor = styleEqual(
                    paste0("Cluster ", seq_len(res$k)),
                    adjustcolor(PALETTE[seq_len(res$k)], alpha.f = 0.25)
                  ),
                  fontWeight = "bold")
  })

  # ============================================================================
  #  ❸ INTERPRÉTATION DYNAMIQUE DES AXES ACP
  # ============================================================================
  output$ui_pca_interpretation <- renderUI({
    res <- analysis()
    req(res)

    # Top-3 variables par contribution absolue sur chaque axe
    var_coord <- as.data.frame(res$pca_result$var$coord)
    top_pc1 <- rownames(var_coord)[order(abs(var_coord$Dim.1), decreasing = TRUE)[1:3]]
    top_pc2 <- rownames(var_coord)[order(abs(var_coord$Dim.2), decreasing = TRUE)[1:3]]

    # Sens de PC1 : TotalSpent positif = gros acheteurs à droite
    label_pc1 <- if (var_coord["TotalSpent", "Dim.1"] > 0)
      "→ droite = <b>gros acheteurs</b> (TotalSpent élevé) | ← gauche = petits acheteurs"
    else
      "← gauche = <b>gros acheteurs</b> (TotalSpent élevé) | → droite = petits acheteurs"

    # Sens de PC2 : Recency positif = clients inactifs en haut
    label_pc2 <- if (var_coord["Recency", "Dim.2"] > 0)
      "↑ haut = <b>clients inactifs</b> (Recency élevée) | ↓ bas = clients récents"
    else
      "↓ bas = <b>clients inactifs</b> (Recency élevée) | ↑ haut = clients récents"

    eig  <- res$eigenvalues
    pct1 <- round(eig[1, 2], 1)
    pct2 <- round(eig[2, 2], 1)
    pct_cum <- round(eig[2, 3], 1)

    HTML(paste0("
    <div style='font-size:14px; line-height:1.9; padding:6px;'>

    <div style='border-left:5px solid #1565C0; padding:10px 16px;
                background:#E3F2FD; border-radius:4px; margin-bottom:14px;'>
      <h4 style='color:#1565C0; margin:0 0 6px 0;'>
        🔵 Axe PC1 &mdash; <b>", pct1, "%</b> de variance expliquée
      </h4>
      <p style='margin:2px 0;'><b>Variables dominantes :</b> ",
        paste(top_pc1, collapse = " &middot; "), "</p>
      <p style='margin:2px 0;'><b>Interprétation :</b>
        PC1 représente l'<b>axe du volume d'achat</b> : il oppose les clients
        à forte valeur économique (dépenses élevées, nombreuses commandes,
        grande quantité) aux petits acheteurs ponctuels.</p>
      <p style='margin:2px 0;'><b>Sens :</b> ", label_pc1, "</p>
    </div>

    <div style='border-left:5px solid #2E7D32; padding:10px 16px;
                background:#E8F5E9; border-radius:4px; margin-bottom:14px;'>
      <h4 style='color:#2E7D32; margin:0 0 6px 0;'>
        🟢 Axe PC2 &mdash; <b>", pct2, "%</b> de variance expliquée
      </h4>
      <p style='margin:2px 0;'><b>Variables dominantes :</b> ",
        paste(top_pc2, collapse = " &middot; "), "</p>
      <p style='margin:2px 0;'><b>Interprétation :</b>
        PC2 représente l'<b>axe de fidélité / récence</b> : il discrimine les
        clients réguliers et récents des clients dormants ou en risque de churn.</p>
      <p style='margin:2px 0;'><b>Sens :</b> ", label_pc2, "</p>
    </div>

    <div style='border-left:5px solid #E65100; padding:10px 16px;
                background:#FFF3E0; border-radius:4px;'>
      <h4 style='color:#E65100; margin:0 0 6px 0;'>
        📌 Synthèse &mdash; Qualité de représentation
      </h4>
      <p style='margin:2px 0;'>Les axes PC1 et PC2 capturent ensemble
        <b>", pct_cum, "%</b> de la variance totale &mdash;
        suffisant pour une visualisation et une segmentation fiables.</p>
      <p style='margin:2px 0;'><b>Règle de Kaiser :</b> on retient les composantes
        dont la valeur propre est &gt; 1 (chaque axe doit expliquer plus
        qu'une variable seule). Le scree plot confirme ce seuil.</p>
    </div>

    </div>"))
  })

  # ============================================================================
  #  ❶ NOMMAGE DYNAMIQUE DES PROFILS (reactive partagé)
  # ============================================================================
  profils_nommes <- reactive({
    res <- analysis()
    req(res)

    # Moyennes par cluster puis Z-score inter-clusters
    moy <- res$customer_df %>%
      group_by(Cluster) %>%
      summarise(across(all_of(VARS_NUM), mean), .groups = "drop")

    moy_z <- moy %>%
      mutate(across(all_of(VARS_NUM), ~ (. - mean(.)) / (sd(.) + 1e-9)))

    # Règles d'assignation basées sur TotalSpent, Recency, NumberInvoices
    nommes <- moy_z %>%
      mutate(Profil = case_when(
        TotalSpent == max(TotalSpent)                          ~ "🏆 VIP / Haute Valeur",
        Recency    == max(Recency)                             ~ "💤 Inactifs / À Risque",
        NumberInvoices == max(NumberInvoices)                  ~ "🔄 Fidèles Réguliers",
        TRUE                                                   ~ "🆕 Nouveaux / Occasionnels"
      ))

    # Sécurité : si k > 4 des profils peuvent être dupliqués → on différencie
    if (anyDuplicated(nommes$Profil) > 0) {
      nommes <- nommes %>%
        mutate(rang = rank(-TotalSpent, ties.method = "first")) %>%
        mutate(Profil = if_else(duplicated(Profil),
                                paste0(Profil, " (", rang, ")"),
                                Profil)) %>%
        select(-rang)
    }
    nommes
  })

  output$ui_profil_nommage <- renderUI({
    res    <- analysis()
    nommes <- profils_nommes()
    req(res, nommes)

    pal    <- PALETTE[seq_len(res$k)]
    counts <- res$customer_df %>% count(Cluster)

    cards <- lapply(seq_len(nrow(nommes)), function(i) {
      row   <- nommes[i, ]
      n_cli <- counts$n[counts$Cluster == row$Cluster]
      pct   <- round(n_cli / nrow(res$customer_df) * 100, 1)
      col   <- pal[i]

      div(style = paste0(
        "border-left:6px solid ", col, "; background:#fff; border-radius:8px;",
        "padding:14px 18px; margin-bottom:12px;",
        "box-shadow:0 2px 6px rgba(0,0,0,0.08);"),
        tags$h4(style = paste0("color:", col, "; margin:0 0 6px 0;"),
                row$Cluster, " — ", row$Profil),
        tags$p(style = "margin:0; font-size:13px; color:#555;",
          icon("users"),   strong(n_cli), " clients (", pct, "%)  |  ",
          icon("coins"),   " TotalSpent Z = ", strong(round(row$TotalSpent, 2)), "  |  ",
          icon("clock"),   " Recency Z = ",    strong(round(row$Recency, 2)),    "  |  ",
          icon("repeat"),  " NbFactures Z = ", strong(round(row$NumberInvoices, 2))
        )
      )
    })

    div(
      p(style = "color:#666; font-size:13px; margin-bottom:14px;",
        icon("circle-info"),
        " Profils assignés automatiquement d'après les Z-scores inter-clusters.",
        " Vérifiez la heatmap (onglet Profils) pour valider la cohérence."),
      do.call(tagList, cards)
    )
  })

  # ============================================================================
  #  ❷ RECOMMANDATIONS MARKETING
  # ============================================================================
  output$table_recommandations <- renderDT({
    nommes <- profils_nommes()
    req(nommes)

    reco_base <- tribble(
      ~Profil_key,               ~Priorite,       ~Actions,                                                                                                                              ~KPI_cible,
      "VIP / Haute Valeur",      "🔴 Critique",   "Programme fidélité Gold/Platinum. Ventes privées exclusives. Service client dédié. Offres personnalisées haute valeur. Événements VIP.", "Réachat > 90% · CLTV > 150% moyenne",
      "Fidèles Réguliers",       "🟠 Haute",      "Newsletter par historique d'achat. Cross-sell & up-sell ciblés. Points fidélité et récompenses. Remise à la Nème commande.",           "Panier moyen +15% · Intervalle achat -10%",
      "Inactifs / À Risque",     "🟡 Urgente",    "Email réactivation + offre retour. SMS promo 48h. Enquête satisfaction. Scoring churn prédictif.",                                    "Taux réactivation > 20% en 3 mois",
      "Nouveaux / Occasionnels", "🟢 Standard",   "Email bienvenue + onboarding. Remise découverte 1re commande. Reco produits populaires. Nurturing 90 jours.",                          "2e achat dans les 60 jours > 30%"
    )

    table_reco <- nommes %>%
      select(Cluster, Profil) %>%
      mutate(Profil_key = Profil %>%
               str_remove("^\\S+ ") %>%   # retirer l'emoji
               str_remove(" \\(\\d+\\)") %>%  # retirer suffixe doublon
               str_trim()) %>%
      left_join(reco_base, by = "Profil_key") %>%
      select(Cluster, Profil, Priorite, Actions, KPI_cible)

    datatable(
      table_reco,
      escape  = FALSE,
      options = list(dom = "t", pageLength = 15, scrollX = TRUE,
                     columnDefs = list(list(width = "35%", targets = 3))),
      class   = "table-striped table-hover table-bordered"
    ) %>%
      formatStyle("Priorite",
                  backgroundColor = styleEqual(
                    c("🔴 Critique","🟠 Haute","🟡 Urgente","🟢 Standard"),
                    c("#FFEBEE",    "#FFF3E0",  "#FFFDE7",   "#E8F5E9")
                  ),
                  fontWeight = "bold")
  })

  # ============================================================================
  #  Synthèse décisionnelle
  # ============================================================================
  output$ui_synthese <- renderUI({
    res    <- analysis()
    nommes <- profils_nommes()
    req(res, nommes)

    n_tot   <- nrow(res$customer_df)
    sil_moy <- round(mean(res$sil_obj[, "sil_width"]), 3)
    eig     <- res$eigenvalues
    pct_acp <- round(eig[2, 3], 1)
    sil_ok  <- if (sil_moy >= 0.40) "✅ acceptable" else "⚠️ à surveiller"

    vip_row   <- nommes %>% arrange(desc(TotalSpent)) %>% slice(1)
    inact_row <- nommes %>% arrange(desc(Recency))    %>% slice(1)
    vip_n     <- res$customer_df %>% filter(Cluster == vip_row$Cluster)   %>% nrow()
    inact_n   <- res$customer_df %>% filter(Cluster == inact_row$Cluster) %>% nrow()
    vip_pct   <- round(vip_n   / n_tot * 100, 1)
    inact_pct <- round(inact_n / n_tot * 100, 1)

    HTML(paste0("
    <div style='font-size:14px; line-height:1.9; padding:6px;'>

    <h4>📊 Résultats quantitatifs</h4>
    <ul>
      <li><b>", n_tot, " clients</b> segmentés en <b>", res$k,
      " clusters</b> (K-Means, seed=42, nstart=25, iter.max=200)</li>
      <li>ACP : PC1 + PC2 = <b>", pct_acp, "%</b> de variance expliquée</li>
      <li>Silhouette moyenne = <b>", sil_moy, "</b> — ", sil_ok, "</li>
    </ul>

    <h4>💡 Priorités d'action métier</h4>
    <ol>
      <li><b>Protéger les ", vip_row$Profil, "</b> — ", vip_n, " clients (", vip_pct,
      "%) : concentrent la majorité du CA. Chaque client perdu a un impact fort.</li>
      <li><b>Réactiver les ", inact_row$Profil, "</b> — ", inact_n, " clients (",
      inact_pct, "%) : campagne ciblée à 20% de taux de succès = gain immédiat.</li>
      <li><b>Convertir les Nouveaux/Occasionnels</b> via onboarding 90 jours.</li>
    </ol>

    <h4>⚠️ Limites du modèle</h4>
    <ul>
      <li>K-Means supposé sphérique : peut mal capturer des formes de clusters allongées</li>
      <li>Fenêtre 1 an : cycles saisonniers non capturés</li>
      <li>Nommage algorithmique : à valider avec la connaissance terrain</li>
      <li>Mise à jour mensuelle recommandée pour suivre les glissements de segment</li>
    </ul>

    </div>"))
  })

} # fin server

# ==============================================================================
#  LANCEMENT
# ==============================================================================
shinyApp(ui = ui, server = server)
