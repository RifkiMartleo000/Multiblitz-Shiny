library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(cluster)
library(MASS)
library(car)
library(psych)
library(readxl)
library(VIM)
library(mice)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      /* Base styling */
      body {
        transition: all 0.3s ease;
        font-family: Bahnschrift;
      }

      /* Default theme */
      .theme-default {
        background: linear-gradient(-45deg, #ee7752, #e73c7e, #23a6d5, #23d5ab);
        background-size: 400% 400%;
        animation: gradientShift 15s ease infinite;
        color: #ffffff; /* White font for high contrast against vibrant gradient */
      }

      /* Light theme */
      .theme-light {
        background: linear-gradient(-45deg, #ff9a9e, #fecfef, #feffff, #ffecd2);
        background-size: 400% 400%;
        animation: gradientShift 12s ease infinite;
        color: #2d3436; /* Dark gray font for contrast against light gradient */
      }

      /* Dark theme */
      .theme-dark {
        background: linear-gradient(-45deg, #2c3e50, #34495e, #2980b9, #8e44ad);
        background-size: 400% 400%;
        animation: gradientShift 18s ease infinite;
        color: #f1f1f1; /* Light gray font for contrast against dark gradient */
      }

      /* Ensure all text elements inherit theme color */
      .theme-default h1, .theme-default h2, .theme-default h3, .theme-default h4,
      .theme-default p, .theme-default label, .theme-default .dataTables_wrapper,
      .theme-default .btn-theme, .theme-default .btn-download {
        color: #ffffff;
      }

      .theme-light h1, .theme-light h2, .theme-light h3, .theme-light h4,
      .theme-light p, .theme-light label, .theme-light .dataTables_wrapper,
      .theme-light .btn-theme, .theme-light .btn-download {
        color: #2d3436;
      }

      .theme-dark h1, .theme-dark h2, .theme-dark h3, .theme-dark h4,
      .theme-dark p, .theme-dark label, .theme-dark .dataTables_wrapper,
      .theme-dark .btn-theme, .theme-dark .btn-download {
        color: #f1f1f1;
      }

      /* Animated background */
      @keyframes gradientShift {
        0% { background-position: 0% 50%; }
        50% { background-position: 100% 50%; }
        100% { background-position: 0% 50%; }
      }

      @keyframes float {
        0%, 100% { transform: translateY(0px) rotate(0deg); opacity: 0; }
        50% { transform: translateY(-100px) rotate(180deg); opacity: 1; }
      }

      /* Content styling */
      .content-wrapper {
        background: rgba(255, 255, 255, 0.1);
        backdrop-filter: blur(5px);
        border-radius: 15px;
        padding: 20px;
        margin: 20px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
        border: 1px solid rgba(255, 255, 255, 0.2);
      }

      .theme-dark .content-wrapper {
        background: rgba(0, 0, 0, 0.2);
        border: 1px solid rgba(255, 255, 255, 0.1);
      }

      /* Control panel styling */
      .control-panel {
        background: rgba(255, 255, 255, 0.1);
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px;
        backdrop-filter: blur(5px);
      }

      .theme-dark .control-panel {
        background: rgba(0, 0, 0, 0.3);
      }

      /* Button styling */
      .btn-theme {
        margin: 5px;
        border-radius: 20px;
        border: none;
        padding: 8px 16px;
        font-weight: bold;
        transition: all 0.3s ease;
        cursor: pointer;
      }

      .btn-default { background: linear-gradient(45deg, #ff6b6b, #4ecdc4); color: #ffffff; }
      .btn-light { background: linear-gradient(45deg, #ffeaa7, #fab1a0); color: #2d3436; }
      .btn-dark { background: linear-gradient(45deg, #2d3436, #636e72); color: #f1f1f1; }

      /* Data table styling */
      .dataTables_wrapper {
        background: rgba(255, 255, 255, 0.1);
        border-radius: 10px;
        padding: 15px;
      }

      /* Plot styling */
      .plotly {
        background: rgba(255, 255, 255, 0.05) !important;
        border-radius: 15px;
      }

      /* Download button styling */
      .download-section {
        background: rgba(255, 255, 255, 0.1);
        border-radius: 10px;
        padding: 15px;
        margin: 15px 0;
        text-align: center;
      }

      .theme-dark .download-section {
        background: rgba(0, 0, 0, 0.2);
      }

      .btn-download {
        background: linear-gradient(45deg, #667eea, #764ba2);
        color: #ffffff;
        border: none;
        border-radius: 25px;
        padding: 12px 24px;
        font-size: 16px;
        font-weight: bold;
        cursor: pointer;
        transition: all 0.3s ease;
        margin: 5px;
      }

      .btn-download:hover {
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(102, 126, 234, 0.4);
      }

      h1, h2, h3, h4 {
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.1);
        margin-bottom: 15px;
      }

      .theme-dark h1, .theme-dark h2, .theme-dark h3, .theme-dark h4 {
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
      }
    "))
  ),
  
  # Main content
  tags$div(
    id = "main-content",
    class = "theme-default",
    style = "min-height: 100vh; transition: all 0.3s ease;",
    
    # Control Panel
    div(class = "control-panel",
        fluidRow(
          column(4,
                 h4("🎨 Pilih Tema"),
                 actionButton("theme_default", "Default", class = "btn-theme btn-default"),
                 actionButton("theme_light", "Light", class = "btn-theme btn-light"),
                 actionButton("theme_dark", "Dark", class = "btn-theme btn-dark")
          ),
          column(4,
                 h4("📏 Ukuran Font"),
                 sliderInput("font_size", "", 
                             min = 10, max = 20, value = 14, step = 1,
                             post = "px")
          ),
          column(4,
                 h4("🚀 Kecepatan Animasi"),
                 sliderInput("animation_speed", "", 
                             min = 5, max = 25, value = 15, step = 2,
                             post = "s")
          )
        )
    ),
    
    # Main content area
    div(class = "content-wrapper",
        titlePanel(h1("Aplikasi Multiblitz", 
                      style = "text-align: center; margin-bottom: 30px;")),
        tabsetPanel(
          tabPanel("ℹ️ About",
                   br(),
                   div(style = "text-align: center; padding: 40px;",
                       h2("Selamat Datang di Aplikasi MultiBlitz!"),
                       h3("Mengungkap pola tersembunyi dalam data multivariat secepat kilat. ⚡"),
                       br(),
                       h4("Panduan penggunaan: "),
                       tags$ol(style = "text-align: left; display: inline-block;",
                               tags$li("Pilih tema, ukuran font, dan kecepatan animasi yang Anda inginkan."),
                               tags$li("Unggah file data Anda (format CSV/Excel) pada tab “Unggah Data”. 
                                       Pastikan data yang Anda unggah sudah benar."),
                               tags$li("Pilih metode analisis yang diperlukan:",
                                       tags$ul(
                                         tags$li("Ringkasan Data ",
                                                 tags$ul(style = "text-align: left;",
                                                         "Menampilkan summary data Anda dan struktur data Anda.")
                                         ),
                                         tags$li("Statistika Deskriptif",
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Pilih variabel yang diperlukan pada 'Pemilihan Variabel'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Menampilkan deskripsi statistik pada 'Statistika Deskriptif' dan plot distribusi pada 'Plot Distribusi'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "c. Anda dapat mengatur fitur pada 'Plot Distribusi' untuk menampilkan output yang Anda inginkan.")
                                         ),
                                         tags$li("Analisis Korelasi",
                                                 tags$ul(style = "text-align: left;",
                                                         "Pilih opsi korelasi pada 'Pilihan Korelasi' yang terdiri dari: "),
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Variabel yang diperlukan pada 'Pemilihan Variabel.'"),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Metode korelasi yang diinginkan pada 'Metode Korelasi'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "Menampilkan matriks korelasi pada 'Matriks Korelasi' dan plot korelasi pada 'Plot Korelasi'.")
                                         ),
                                         tags$li("PCA",
                                                 tags$ul(style = "text-align: left;",
                                                         "Pilih opsi PCA pada 'Pilihan PCA' yang terdiri dari: "),
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Variabel yang diperlukan pada 'Pemilihan Variabel.'"),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Menyamakan skala variabel dengan mencentang opsi 'Skala Variabel'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "c. Jumlah komponen yang diinginkan pada 'Banyak komponen untuk dilihat'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "Menampilkan ringkasan PCA pada “Ringkasan PCA”, Scree Plot, dan Biplot.")
                                         ),
                                         tags$li("Analisis Klaster",
                                                 tags$ul(style = "text-align: left;",
                                                         "Pilih opsi Klaster pada 'Pilihan Klaster' yang terdiri dari:"),
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Variabel yang diperlukan pada pada 'Pemilihan Variabel'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Metode yang digunakan pada 'Metode Klasterisasi'.",
                                                         tags$ul(style = "text-align: left;",
                                                                 "i. Jika memilih 'K-means', maka ketikkan 'Banyak klaster yang diinginkan' untuk memilih jumlah klaster dan menceklis opsi 'Skala Variabel' untuk menyamakan skala variabel."),
                                                         tags$ul(style = "text-align: left;",
                                                                 "ii. Jika memilih 'Hierarchical', maka pilih metode yang ingin digunakan dan menceklis opsi 'Skala Variabel' untuk menyamakan skala variabel. ")
                                                 ),
                                                 tags$ul(style = "text-align: left;",
                                                         "Menampilkan hasil pada 'Hasil Klaster' dan 'Visualisasi Klaster'.")
                                         ),
                                         tags$li("MANOVA",
                                                 tags$ul(style = "text-align: left;",
                                                         "Pilih opsi MANOVA pada 'Pilihan MANOVA' yang terdiri dari:"),
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Kelompok variabel yang diperlukan pada 'Variabel Kelompok'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Variabel dependen yang diinginkan pada 'Variabel Dependen'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "Menampilkan hasil MANOVA pada ”Hasil MANOVA”.")
                                         )
                                       )),
                               tags$li("Anda dapat mengunduh hasil analisis data anda.")
                       ),
                       br(),
                       h4("Mari mengeksplor dataset anda!")
                   )
          ),
          
          tabPanel("📁 Unggah Data",
                   fluidRow(
                     box(
                       title = "Unggah Dokumen Data Anda", status = "primary", solidHeader = TRUE, width = 12,
                       fileInput("file", "Pilih Dokumen CSV/Excel",
                                 accept = c(".csv", ".xlsx", ".xls")),
                       
                       conditionalPanel(
                         condition = "input.file != null",
                         checkboxInput("header", "Header", TRUE),
                         checkboxInput("stringsAsFactors", "Strings as factors", FALSE),
                         radioButtons("sep", "Separator",
                                      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                      selected = ","),
                         radioButtons("quote", "Quote",
                                      choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                                      selected = '"')
                       ),
                       
                       hr(),
                       h4("Pratinjau Data:"),
                       DT::dataTableOutput("preview")
                     )
                   )
          ),
          
          tabPanel("📊 Ringkasan Data",
                   fluidRow(
                     box(
                       title = "Ringkasan Dataset", status = "info", solidHeader = TRUE, width = 6,
                       verbatimTextOutput("summary")
                     ),
                     box(
                       title = "Struktur Dataset", status = "info", solidHeader = TRUE, 
                       background = "blue",
                       width = 6,
                       verbatimTextOutput("structure")
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Dataset Lengkap", status = "primary", solidHeader = TRUE, width = 12,
                       DT::dataTableOutput("fulldata")
                     )
                   )
          ),
          
          tabPanel("📊 Statistika Deskriptif",
                   fluidRow(
                     box(
                       title = "Pilihan Variabel", status = "primary", solidHeader = TRUE, width = 3,
                       uiOutput("desc_vars"), style = "background-color: transparent;"
                     ),
                     box(
                       title = "Statistika Deskriptif", status = "info", solidHeader = TRUE, width = 9,
                       verbatimTextOutput("descriptive_stats")
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Plot Distribusi", status = "success", solidHeader = TRUE, width = 12,
                       plotlyOutput("distribution_plots", height = "600px")
                     )
                   )
          ),
          
          tabPanel("🔁 Analisis Korelasi",
                   fluidRow(
                     box(
                       title = "Pilihan Korelasi", status = "primary", solidHeader = TRUE, width = 3,
                       uiOutput("corr_vars"),
                       radioButtons("corr_method", "Metode Korelasi:",
                                    choices = list("Pearson" = "pearson", 
                                                   "Spearman" = "spearman", 
                                                   "Kendall" = "kendall"),
                                    selected = "pearson")
                     ),
                     box(
                       title = "Matriks Korelasi", status = "info", solidHeader = TRUE, width = 9,
                       verbatimTextOutput("correlation_matrix")
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Plot Korelasi", status = "success", solidHeader = TRUE, width = 12,
                       plotOutput("correlation_plot", height = "600px")
                     )
                   )
          ),
          
          tabPanel("⛓ PCA",
                   fluidRow(
                     box(
                       title = "Pilihan PCA", status = "primary", solidHeader = TRUE, width = 3,
                       uiOutput("pca_vars"),
                       checkboxInput("pca_scale", "Skala variabel", TRUE),
                       numericInput("pca_components", "Banyak komponen untuk dilihat:", 
                                    value = 2, min = 1, max = 10)
                     ),
                     box(
                       title = "Ringkasan PCA", status = "info", solidHeader = TRUE, width = 9,
                       verbatimTextOutput("pca_summary")
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Scree Plot", status = "success", solidHeader = TRUE, width = 6,
                       plotOutput("scree_plot")
                     ),
                     box(
                       title = "Biplot", status = "success", solidHeader = TRUE, width = 6,
                       plotOutput("pca_biplot")
                     )
                   )
          ),
          
          tabPanel("🔗 Analisis Klaster",
                   fluidRow(
                     box(
                       title = "Pilihan Klaster", status = "primary", solidHeader = TRUE, width = 3,
                       uiOutput("cluster_vars"),
                       radioButtons("cluster_method", "Metode Klasterisasi:",
                                    choices = list("K-means" = "kmeans", "Hierarchical" = "hclust"),
                                    selected = "kmeans"),
                       conditionalPanel(
                         condition = "input.cluster_method == 'kmeans'",
                         numericInput("k_clusters", "Banyak klaster yang diinginkan:", value = 3, min = 2, max = 10)
                       ),
                       conditionalPanel(
                         condition = "input.cluster_method == 'hclust'",
                         radioButtons("linkage", "Linkage method:",
                                      choices = list("Complete" = "complete", "Average" = "average", 
                                                     "Single" = "single", "Ward" = "ward.D2"),
                                      selected = "complete")
                       ),
                       checkboxInput("cluster_scale", "Skala Variabel", TRUE)
                     ),
                     box(
                       title = "Hasil Klasterisasi", status = "info", solidHeader = TRUE, width = 9,
                       verbatimTextOutput("cluster_summary")
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Visualisasi Klaster", status = "success", solidHeader = TRUE, width = 12,
                       plotOutput("cluster_plot", height = "600px")
                     )
                   )
          ),
          
          tabPanel("🔍 MANOVA",
                   fluidRow(
                     box(
                       title = "Pilihan MANOVA", status = "primary", solidHeader = TRUE, width = 3,
                       uiOutput("manova_group_var"),
                       uiOutput("manova_dep_vars")
                     ),
                     box(
                       title = "Hasil MANOVA", status = "info", solidHeader = TRUE, width = 9,
                       verbatimTextOutput("manova_results")
                     )
                   )
          ),
          
          tabPanel("📥 Unduh Hasil",
                   br(),
                   div(class = "download-section",
                       h4("📊 File Visualisasi Data Anda Sudah Siap!"),
                       p("Unduh hasil visualisasi dan analisis data anda dalam format PDF."),
                       downloadButton("download_custom", "📈 Unduh Laporan Visualisasi", 
                                      class = "btn-download")
                   )
          )
        )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(data = NULL, imputed_data = NULL)
  
  # File upload
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if(ext == "csv") {
      values$data <- read.csv(input$file$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote,
                              stringsAsFactors = input$stringsAsFactors)
    } else if(ext %in% c("xlsx", "xls")) {
      values$data <- read_excel(input$file$datapath)
    }
  })
  
  # Data preview
  output$preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(head(values$data, 100), options = list(scrollX = TRUE))
  })
  
  # Data overview
  output$summary <- renderPrint({
    req(values$data)
    summary(values$data)
  })
  
  output$structure <- renderPrint({
    req(values$data)
    str(values$data)
  })
  
  output$fulldata <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, options = list(scrollX = TRUE))
  })
  
  # Variable selection UI elements
  output$desc_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("selected_desc_vars", "Pilih Variabel:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars[1:min(5, length(numeric_vars))])
  })
  
  output$corr_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("selected_corr_vars", "Pilih Variabel:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars)
  })
  
  output$pca_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("selected_pca_vars", "Pilih Variabel:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars)
  })
  
  output$factor_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("selected_factor_vars", "Pilih Variabel:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars)
  })
  
  output$cluster_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("selected_cluster_vars", "Pilih Variabel:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars)
  })
  
  output$lda_group_var <- renderUI({
    req(values$data)
    factor_vars <- names(values$data)[sapply(values$data, function(x) is.factor(x) || is.character(x))]
    selectInput("lda_group", "Group Variable:", choices = factor_vars)
  })
  
  output$lda_pred_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("lda_predictors", "Variabel Prediktor:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars)
  })
  
  output$manova_group_var <- renderUI({
    req(values$data)
    factor_vars <- names(values$data)[sapply(values$data, function(x) is.factor(x) || is.character(x))]
    selectInput("manova_group", "Variabel Kelompok:", choices = factor_vars)
  })
  
  output$manova_dep_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("manova_dependent", "Variabel Dependen:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars[1:min(3, length(numeric_vars))])
  })
  
  
  
  # Descriptive Statistics
  output$descriptive_stats <- renderPrint({
    req(values$data, input$selected_desc_vars)
    describe(values$data[, input$selected_desc_vars, drop = FALSE])
  })
  
  output$distribution_plots <- renderPlotly({
    req(values$data, input$selected_desc_vars)
    data_subset <- values$data[, input$selected_desc_vars, drop = FALSE]
    
    plots <- list()
    for(i in 1:ncol(data_subset)) {
      p <- plot_ly(x = ~data_subset[,i], type = "histogram", name = names(data_subset)[i]) %>%
        layout(title = paste("Distribusi dari", names(data_subset)[i]), 
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)'
        )
      plots[[i]] <- p
    }
    
    subplot(plots, nrows = ceiling(length(plots)/2))
  })
  
  # Correlation Analysis
  output$correlation_matrix <- renderPrint({
    req(values$data, input$selected_corr_vars)
    cor(values$data[, input$selected_corr_vars], method = input$corr_method, use = "complete.obs")
  })
  
  output$correlation_plot <- renderPlot({
    req(values$data, input$selected_corr_vars)
    corr_matrix <- cor(values$data[, input$selected_corr_vars], method = input$corr_method, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", bg = NULL,
             tl.cex = 0.8, tl.col = "black", tl.srt = 45)
  })
  
  # PCA Analysis
  output$pca_summary <- renderPrint({
    req(values$data, input$selected_pca_vars)
    pca_data <- values$data[, input$selected_pca_vars]
    pca_data <- pca_data[complete.cases(pca_data), ]
    
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    summary(pca_result)
  })
  
  output$scree_plot <- renderPlot({
    req(values$data, input$selected_pca_vars)
    pca_data <- values$data[, input$selected_pca_vars]
    pca_data <- pca_data[complete.cases(pca_data), ]
    
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    fviz_eig(pca_result, addlabels = TRUE)
  })
  
  output$pca_biplot <- renderPlot({
    req(values$data, input$selected_pca_vars)
    pca_data <- values$data[, input$selected_pca_vars]
    pca_data <- pca_data[complete.cases(pca_data), ]
    
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    fviz_pca_biplot(pca_result, repel = TRUE)
  })
  
  # Cluster Analysis
  output$cluster_summary <- renderPrint({
    req(values$data, input$selected_cluster_vars)
    cluster_data <- values$data[, input$selected_cluster_vars]
    cluster_data <- cluster_data[complete.cases(cluster_data), ]
    
    if(input$cluster_scale) {
      cluster_data <- scale(cluster_data)
    }
    
    if(input$cluster_method == "kmeans") {
      cluster_result <- kmeans(cluster_data, centers = input$k_clusters, nstart = 25)
      print(cluster_result)
    } else {
      dist_matrix <- dist(cluster_data)
      cluster_result <- hclust(dist_matrix, method = input$linkage)
      print(cluster_result)
    }
  })
  
  output$cluster_plot <- renderPlot({
    req(values$data, input$selected_cluster_vars)
    cluster_data <- values$data[, input$selected_cluster_vars]
    cluster_data <- cluster_data[complete.cases(cluster_data), ]
    
    if(input$cluster_scale) {
      cluster_data <- scale(cluster_data)
    }
    
    if(input$cluster_method == "kmeans") {
      cluster_result <- kmeans(cluster_data, centers = input$k_clusters, nstart = 25)
      fviz_cluster(cluster_result, data = cluster_data)
    } else {
      dist_matrix <- dist(cluster_data)
      cluster_result <- hclust(dist_matrix, method = input$linkage)
      fviz_dend(cluster_result, k = 4, cex = 0.5)
    }
  })
  
  # MANOVA
  output$manova_results <- renderPrint({
    req(values$data, input$manova_group, input$manova_dependent)
    manova_data <- values$data[, c(input$manova_group, input$manova_dependent)]
    manova_data <- manova_data[complete.cases(manova_data), ]
    
    dep_vars <- paste("cbind(", paste(input$manova_dependent, collapse = ", "), ")")
    formula_str <- paste(dep_vars, "~", input$manova_group)
    manova_result <- manova(as.formula(formula_str), data = manova_data)
    summary(manova_result)
  })
  
  
  
  
  
  
  # Generate sample data
  sample_data <- reactive({
    n <- if(is.null(input$n_points)) 50 else input$n_points
    data.frame(
      x = 1:n,
      y = cumsum(rnorm(n, 0, 1)) + rnorm(n, 0, 0.1),
      category = sample(c("A", "B", "C"), n, replace = TRUE),
      value = rnorm(n, 100, 15)
    )
  })
  
  # Theme switching
  observeEvent(input$theme_default, {
    runjs("document.getElementById('main-content').className = 'theme-default';")
  })
  
  observeEvent(input$theme_light, {
    runjs("document.getElementById('main-content').className = 'theme-light';")
  })
  
  observeEvent(input$theme_dark, {
    runjs("document.getElementById('main-content').className = 'theme-dark';")
  })
  
  # Font size adjustment
  observeEvent(input$font_size, {
    runjs(paste0("document.getElementById('main-content').style.fontSize = '", input$font_size, "px';"))
  })
  
  # Animation speed adjustment
  observeEvent(input$animation_speed, {
    runjs(paste0("
      const style = document.createElement('style');
      style.textContent = `
        .theme-default, .theme-light, .theme-dark {
          animation-duration: ", input$animation_speed, "s !important;
        }
      `;
      document.head.appendChild(style);
    "))
  })
  
  # Interactive plot
  output$interactive_plot <- renderPlotly({
    data <- sample_data()
    
    p <- plot_ly(data, x = ~x, y = ~y, color = ~category,
                 type = 'scatter', mode = 'markers+lines',
                 marker = list(size = 8, opacity = 0.7),
                 line = list(width = 2)) %>%
      layout(
        title = "Interactive Time Series",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = 'white')
      )
    
    p
  })
  
  # Dynamic plot based on controls
  output$dynamic_plot <- renderPlotly({
    data <- sample_data()
    
    if(input$plot_type == "scatter") {
      p <- plot_ly(data, x = ~x, y = ~value, color = ~category,
                   type = 'scatter', mode = 'markers',
                   marker = list(size = 10, opacity = 0.8))
    } else if(input$plot_type == "line") {
      p <- plot_ly(data, x = ~x, y = ~value, color = ~category,
                   type = 'scatter', mode = 'lines+markers',
                   line = list(width = 3))
    } else {
      agg_data <- data %>% group_by(category) %>% summarise(avg_value = mean(value))
      p <- plot_ly(agg_data, x = ~category, y = ~avg_value,
                   type = 'bar', marker = list(opacity = 0.8))
    }
    
    if(input$show_trend && input$plot_type != "bar") {
      p <- p %>% add_lines(data = data, x = ~x, y = ~fitted(lm(value ~ x, data = data)),
                           name = "Trend", line = list(color = "red", width = 2, dash = "dash"))
    }
    
    p %>% layout(
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      font = list(color = 'white')
    )
  })
  
  # Sample data table
  output$sample_table <- DT::renderDataTable({
    datatable(sample_data(), 
              options = list(pageLength = 8, scrollX = TRUE),
              style = "bootstrap",
              class = "table-striped table-hover")
  })
  
  # Generate static plots for PDF
  generate_static_plot <- function(data, type = "basic") {
    if(type == "basic") {
      ggplot(data, aes(x = x, y = y, color = category)) +
        geom_point(size = 3, alpha = 0.7) +
        geom_line(size = 1.2) +
        labs(title = "Interactive Time Series Analysis",
             x = "Time Points", y = "Values",
             color = "Category") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              legend.position = "bottom")
    } else {
      if(input$plot_type == "scatter") {
        p <- ggplot(data, aes(x = x, y = value, color = category)) +
          geom_point(size = 3, alpha = 0.8) +
          labs(title = "Scatter Plot Analysis", x = "X Values", y = "Y Values")
      } else if(input$plot_type == "line") {
        p <- ggplot(data, aes(x = x, y = value, color = category)) +
          geom_line(size = 1.2) + geom_point(size = 2) +
          labs(title = "Line Plot Analysis", x = "X Values", y = "Y Values")
      } else {
        agg_data <- data %>% group_by(category) %>% summarise(avg_value = mean(value))
        p <- ggplot(agg_data, aes(x = category, y = avg_value, fill = category)) +
          geom_bar(stat = "identity", alpha = 0.8) +
          labs(title = "Bar Chart Analysis", x = "Category", y = "Average Value")
      }
      
      p + theme_minimal() +
        theme(plot.title = element_text(size = 16, hjust = 0.5),
              legend.position = "bottom")
    }
  }
  
  # Download handler for basic report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Data_Analysis_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create temporary RMD file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Create RMD content
      rmd_content <- paste0('
---
title: "Data Analysis Report"
subtitle: "Generated from Dynamic Shiny Dashboard"
author: "Dynamic Dashboard App"
date: "`r Sys.Date()`"
output: 
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
geometry: margin=1in
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 5)
library(ggplot2)
library(dplyr)
library(knitr)
```

# Executive Summary

This report provides a comprehensive analysis of the dataset generated in the Dynamic Shiny Dashboard. The analysis includes data visualization, statistical summaries, and key insights.

# Dataset Overview

```{r data-summary}
data <- ', deparse(substitute(sample_data())), '
knitr::kable(head(values, 10), caption = "Sample Data (First 10 Rows)")
```

## Statistical Summary

```{r stats-summary}
summary_stats <- data %>%
  group_by(category) %>%
  summarise(
    Count = n(),
    Mean_Y = round(mean(y), 2),
    SD_Y = round(sd(y), 2),
    Mean_Value = round(mean(value), 2),
    SD_Value = round(sd(value), 2)
  )
knitr::kable(summary_stats, caption = "Statistical Summary by Category")
```

# Data Visualization

## Time Series Analysis

```{r plot1, fig.cap="Time Series Plot showing trends over time"}
', deparse(substitute(generate_static_plot(sample_data(), "basic"))), '
```

## Distribution Analysis

```{r plot2, fig.cap="Distribution of values by category"}
ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Values by Category",
       x = "Category", y = "Values") +
  theme_minimal() +
  theme(legend.position = "none")
```

## Correlation Analysis

```{r correlation}
cor_matrix <- cor(data[, c("x", "y", "value")])
knitr::kable(round(cor_matrix, 3), caption = "Correlation Matrix")
```

# Key Insights

- **Total Observations**: `r nrow(data)`
- **Categories**: `r length(unique(data$category))`
- **Data Range**: X values from `r min(data$x)` to `r max(data$x)`
- **Average Y Value**: `r round(mean(data$y), 2)`
- **Average Value**: `r round(mean(data$value), 2)`

## Recommendations

1. The data shows interesting patterns across different categories
2. Time series trends indicate potential seasonal or cyclical behavior
3. Further analysis could explore predictive modeling opportunities

---

*Report generated on `r Sys.time()` using the Dynamic Shiny Dashboard*
')
      
      # Write RMD content to file
      writeLines(rmd_content, temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  # Download handler for custom report
  output$download_custom <- downloadHandler(
    filename = function() {
      paste("Custom_Analysis_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      plot_type_title <- switch(input$plot_type,
                                "scatter" = "Scatter Plot",
                                "line" = "Line Chart", 
                                "bar" = "Bar Chart")
      
      rmd_content <- paste0('
---
title: "Custom Analysis Report"
subtitle: "', plot_type_title, ' Analysis"
author: "Dynamic Dashboard App"
date: "`r Sys.Date()`"
output: 
  pdf_document:
    toc: true
    number_sections: true
geometry: margin=1in
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6)
library(ggplot2)
library(dplyr)
library(knitr)
```

# Analysis Configuration

- **Plot Type**: ', plot_type_title, '
- **Number of Data Points**: ', input$n_points, '
- **Trend Line**: ', ifelse(input$show_trend, "Enabled", "Disabled"), '
- **Generated**: `r Sys.time()`

# Custom Visualization

```{r custom-plot, fig.cap="Custom visualization based on user settings"}
', deparse(substitute(generate_static_plot(sample_data(), "custom"))), '
```

# Data Analysis

```{r data-table}
data <- ', deparse(substitute(sample_data())), '
knitr::kable(head(data, 15), caption = "Dataset Sample")
```

# Statistical Summary

```{r custom-stats}
if("', input$plot_type, '" != "bar") {
  cat("## Trend Analysis\\n")
  if(', input$show_trend, ') {
    model <- lm(value ~ x, data = data)
    cat("- **Slope**: ", round(coef(model)[2], 4), "\\n")
    cat("- **R-squared**: ", round(summary(model)$r.squared, 4), "\\n")
    cat("- **P-value**: ", round(summary(model)$coefficients[2,4], 4), "\\n")
  }
}

summary_by_category <- data %>%
  group_by(category) %>%
  summarise(
    Count = n(),
    Mean = round(mean(value), 2),
    Median = round(median(value), 2),
    SD = round(sd(value), 2),
    Min = round(min(value), 2),
    Max = round(max(value), 2)
  )

knitr::kable(summary_by_category, caption = "Summary Statistics by Category")
```

---

*Custom report generated from Dynamic Shiny Dashboard*
')
      
      writeLines(rmd_content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)