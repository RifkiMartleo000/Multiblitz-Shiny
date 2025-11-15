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
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        transition: all 0.3s ease;
        font-family: Bahnschrift;
      }
      .theme-default {
        background: linear-gradient(-45deg, #ee7752, #e73c7e, #23a6d5, #23d5ab);
        background-size: 400% 400%;
        animation: gradientShift 15s ease infinite;
        color: #333333;
      }
      .theme-light {
        background: linear-gradient(-45deg, #ff9a9e, #fecfef, #feffff, #ffecd2);
        background-size: 400% 400%;
        animation: gradientShift 12s ease infinite;
        color: #444444;
      }
      .theme-dark {
        background: linear-gradient(-45deg, #2c3e50, #34495e, #2980b9, #8e44ad);
        background-size: 400% 400%;
        animation: gradientShift 18s ease infinite;
        color: #ecf0f1;
      }
      @keyframes gradientShift {
        0% { background-position: 0% 50%; }
        50% { background-position: 100% 50%; }
        100% { background-position: 0% 50%; }
      }
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
      .control-panel {
        background: rgba(255, 255, 255, 0.1);
        border-radius: 10px;
        padding: 15px;
        margin: 20px 20px 0 20px; /* Adjusted margin */
        backdrop-filter: blur(5px);
      }
      .theme-dark .control-panel {
        background: rgba(0, 0, 0, 0.3);
      }
      .btn-theme {
        margin: 5px;
        border-radius: 20px;
        border: none;
        padding: 8px 16px;
        font-weight: bold;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      .btn-default { background: linear-gradient(45deg, #ff6b6b, #4ecdc4); color: white; }
      .btn-light { background: linear-gradient(45deg, #ffeaa7, #fab1a0); color: #2d3436; }
      .btn-dark { background: linear-gradient(45deg, #2d3436, #636e72); color: white; }
      .dataTables_wrapper {
        background: rgba(255, 255, 255, 0.1);
        border-radius: 10px;
        padding: 15px;
      }
      .plotly {
        background: rgba(255, 255, 255, 0.05) !important;
        border-radius: 15px;
      }
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
        color: white;
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
      
      /* MODIFIKASI: Atur ukuran font judul tabPanel */
      .nav-tabs > li > a {
        font-size: 15px !important;
      }
      
      /* Atur warna font tabPanel sesuai tema */
      .theme-default .nav-tabs > li > a {
        color: #444444 !important;
      }
      
      .theme-light .nav-tabs > li > a {
        color: #444444 !important;
      }
            
      /* Warna teks tabPanel */
      .theme-dark .nav-tabs > li > a {
        color: #ecf0f1 !important;  /* Teks lebih cerah agar jelas */
      }

      /* Warna teks dan background saat hover */
      .theme-dark .nav-tabs > li > a:hover {
        color: #ffffff !important;  /* Teks tetap putih */
        background-color: #50565c !important;  /* Warna abu gelap agar kontras */
        border-color: #ffffff !important;  /* Border tetap terlihat */
      }

      /* Warna teks dan background saat tab aktif */
      .theme-dark .nav-tabs > li.active > a {
        color: #ffffff !important;  /* Teks gelap agar kontras */
        background-color: #3f4449 !important;  /* Warna kuning agar terlihat */
        border-color: #ffffff !important;  /* Border mengikuti warna background */
      }
      
      /* Ubah warna teks pada DataTable dalam mode dark */
      .theme-dark .dataTables_wrapper {
        color: #ffffff !important;  /* Warna teks menjadi putih */
        background-color: rgba(0, 0, 0, 0.3) !important; /* Background gelap */
      }

      /* Ubah warna teks pada output verbatim (Ringkasan Data, Struktur Data) */
      .theme-dark .shiny-text-output {
        color: #ffffff !important;  /* Teks menjadi putih agar mudah dibaca */
        background-color: rgba(0, 0, 0, 0.3) !important; /* Background sedikit gelap */
        border-radius: 5px;
        padding: 10px;
      }

      /* Ubah warna teks dan background pada Plot Output */
      .theme-dark .plotly {
        color: #ffffff !important; /* Warna teks grafik putih agar terlihat */
        background-color: rgba(0, 0, 0, 0.3) !important; /* Background lebih gelap */
      }

      /* Border dan kontras pada tabel dataset lengkap */
      .theme-dark .DT {
        color: #ffffff !important;
        background-color: rgba(0, 0, 0, 0.3) !important;
        border: 1px solid rgba(255, 255, 255, 0.4) !important;
      }
      .upload-prompt {
        text-align: center;
        padding: 50px;
        font-size: 1.2em;
        font-weight: bold;
      }
      
    "))
  ),
  
  tags$div(
    id = "main-content",
    class = "theme-default",
    style = "min-height: 100vh; transition: all 0.3s ease;",
    
    div(class = "control-panel",
        fluidRow(
          style = "display: flex; align-items: center;", 
          
          # Column 1: Application Title
          column(4, style = "margin-left: 50px;",
                 div(style = "display: flex; align-items: center; gap: 15px;",
                     tags$img(src = "logo1.png", height = "75px"),
                     h1("Multiblitz", style = "margin: 0; white-space: nowrap;")
                 )
          ),
          # Column 2: Interactive Controls
          column(12,
                 fluidRow(
                   column(4,
                          h4("🎨 Pilih Tema", style = "margin-top: 50;"),
                          actionButton("theme_default", span(icon("sun"), "Default"), class = "btn-theme btn-default"),
                          actionButton("theme_light", span(icon("lightbulb"), "Light"), class = "btn-theme btn-light"),
                          actionButton("theme_dark", span(icon("moon"), "Dark"), class = "btn-theme btn-dark")
                   ),
                   column(4,
                          h4("📏 Ukuran Font", style = "margin-top: 0;"),
                          sliderInput("font_size", "", min = 18, max = 28, value = 20, step = 1, post = "px")
                   ),
                   column(4,
                          h4("🚀 Kecepatan Animasi", style = "margin-top: 0;"),
                          sliderInput("animation_speed", "", min = 5, max = 25, value = 15, step = 2, post = "s")
                   )
                 )
          )
        )
    ),
    
    div(class = "content-wrapper",
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
                                                         "c. Menampilkan matriks korelasi pada 'Matriks Korelasi' dan plot korelasi pada 'Plot Korelasi'.")
                                         ),
                                         tags$li("PCA",
                                                 tags$ul(style = "text-align: left;",
                                                         "Pilih opsi PCA pada 'Pilihan PCA' yang terdiri dari: "),
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Variabel yang diperlukan pada 'Pemilihan Variabel.'"),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Menyamakan skala variabel dengan mencentang opsi 'Skala Variabel'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "c. Menampilkan ringkasan PCA pada “Ringkasan PCA”, Scree Plot, dan Biplot.")
                                         ),
                                         tags$li("Analisis Klaster",
                                                 tags$ul(style = "text-align: left;",
                                                         "Pilih opsi Klaster pada 'Pilihan Klaster' yang terdiri dari:"),
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Variabel yang diperlukan pada pada 'Pemilihan Variabel'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Tentukan jumlah klaster optimal dengan menekan tombol 'Tampilkan Plot Elbow & Silhouette'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "c. Pilih metode yang digunakan pada 'Metode Klasterisasi'.",
                                                         tags$ul(style = "text-align: left;",
                                                                 "i. Jika memilih 'K-means', maka ketikkan 'Banyak klaster yang diinginkan' untuk memilih jumlah klaster dan menceklis opsi 'Skala Variabel' untuk menyamakan skala variabel."),
                                                         tags$ul(style = "text-align: left;",
                                                                 "ii. Jika memilih 'Hierarchical', maka pilih metode yang ingin digunakan dan menceklis opsi 'Skala Variabel' untuk menyamakan skala variabel. ")
                                                 ),
                                                 tags$ul(style = "text-align: left;",
                                                         "d. Menampilkan hasil pada 'Hasil Klaster' dan 'Visualisasi Klaster'.")
                                         ),
                                         tags$li("MANOVA",
                                                 tags$ul(style = "text-align: left;",
                                                         "Pilih opsi MANOVA pada 'Pilihan MANOVA' yang terdiri dari:"),
                                                 tags$ul(style = "text-align: left;",
                                                         "a. Kelompok variabel yang diperlukan pada 'Variabel Kelompok'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "b. Variabel dependen yang diinginkan pada 'Variabel Dependen'."),
                                                 tags$ul(style = "text-align: left;",
                                                         "c. Menampilkan hasil MANOVA pada ”Hasil MANOVA”.")
                                         )
                                       )),
                               tags$li("Anda dapat mengunduh hasil analisis data anda.")
                       ),
                       br(),
                       h4("Mari mengeksplor dataset anda!")
                   )
          ),
          tabPanel("👥 Tim Kami",
                   br(),
                   div(style = "padding: 40px;",
                       h2("Kelompok 4", style = "text-align: center; margin-bottom: 40px;"),
                       fluidRow(
                         column(1),
                         column(2, style = "text-align: center;",
                                tags$div(icon("user-circle", class = "fa-5x"), style="margin-bottom: 15px;"),
                                h4("Andika Verda Madyana"),
                                p("M0723010")
                         ),
                         column(2, style = "text-align: center;",
                                tags$div(icon("user-circle", class = "fa-5x"), style="margin-bottom: 15px;"),
                                h4("Ema Nur Kamila"),
                                p("M0723030")
                         ),
                         column(2, style = "text-align: center;",
                                tags$div(icon("user-circle", class = "fa-5x"), style="margin-bottom: 15px;"),
                                h4("Nayla Rahma Ridhafasya"),
                                p("M0723064")
                         ),
                         column(2, style = "text-align: center;",
                                tags$div(icon("user-circle", class = "fa-5x"), style="margin-bottom: 15px;"),
                                h4("Rifki Martleo Alfiansyah"),
                                p("M0723076")
                         ),
                         column(2, style = "text-align: center;",
                                tags$div(icon("user-circle", class = "fa-5x"), style="margin-bottom: 15px;"),
                                h4("Salsabila Arij Aulia"),
                                p("M0723082")
                         )
                       )
                   )
          ),
          # --- END OF NEW TAB PANEL ---
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
                   conditionalPanel(
                     condition = "!output.fileUploaded",
                     div(class = "upload-prompt", "Unggah file terlebih dahulu untuk melihat analisis.")
                   ),
                   conditionalPanel(
                     condition = "output.fileUploaded",
                     fluidRow(
                       box(
                         title = "Ringkasan Dataset", status = "info", solidHeader = TRUE, width = 12,
                         DT::dataTableOutput("summary")
                       ),
                       box(
                         title = "Struktur Dataset", status = "info", solidHeader = TRUE, background = "blue", width = 12,
                         DT::dataTableOutput("structure")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Dataset Lengkap", status = "primary", solidHeader = TRUE, width = 12,
                         DT::dataTableOutput("fulldata")
                       )
                     )
                   )
          ),
          tabPanel("📊 Statistika Deskriptif",
                   conditionalPanel(
                     condition = "!output.fileUploaded",
                     div(class = "upload-prompt", "Unggah file terlebih dahulu untuk melihat analisis.")
                   ),
                   conditionalPanel(
                     condition = "output.fileUploaded",
                     fluidRow(
                       box(
                         title = "Pilihan Variabel", status = "primary", solidHeader = TRUE, width = 3,
                         uiOutput("desc_vars"), style = "background-color: transparent;"
                       ),
                       box(
                         title = "Statistika Deskriptif", status = "info", solidHeader = TRUE, width = 12,
                         DT::dataTableOutput("descriptive_stats")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Plot Distribusi", status = "success", solidHeader = TRUE, width = 12,
                         plotlyOutput("distribution_plots", height = "600px")
                       )
                     )
                   )
          ),
          tabPanel("🔁 Analisis Korelasi",
                   conditionalPanel(
                     condition = "!output.fileUploaded",
                     div(class = "upload-prompt", "Unggah file terlebih dahulu untuk melihat analisis.")
                   ),
                   conditionalPanel(
                     condition = "output.fileUploaded",
                     fluidRow(
                       box(
                         title = "Pilihan Korelasi", status = "primary", solidHeader = TRUE, width = 3,
                         uiOutput("corr_vars"),
                         radioButtons("corr_method", "Metode Korelasi:",
                                      choices = list("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
                                      selected = "pearson")
                       ),
                       box(
                         title = "Matriks Korelasi", status = "info", solidHeader = TRUE, width = 9,
                         DT::dataTableOutput("correlation_matrix")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Plot Korelasi", status = "success", solidHeader = TRUE, width = 12,
                         plotOutput("correlation_plot", height = "600px")
                       )
                     )
                   )
          ),
          tabPanel("⛓ PCA",
                   conditionalPanel(
                     condition = "!output.fileUploaded",
                     div(class = "upload-prompt", "Unggah file terlebih dahulu untuk melihat analisis.")
                   ),
                   conditionalPanel(
                     condition = "output.fileUploaded",
                     fluidRow(
                       box(
                         title = "Pilihan PCA", status = "primary", solidHeader = TRUE, width = 3,
                         uiOutput("pca_vars"),
                         checkboxInput("pca_scale", "Skala variabel", TRUE)
                       ),
                       box(
                         title = "Ringkasan PCA", status = "info", solidHeader = TRUE, width = 9,
                         tabsetPanel(
                           tabPanel("Komponen Utama", DT::dataTableOutput("pca_eigenvalues")),
                           tabPanel("Variabel", DT::dataTableOutput("pca_varloadings")),
                           tabPanel("Individu", DT::dataTableOutput("pca_indiv_scores"))
                         )
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
                   )
          ),
          tabPanel("🔗 Analisis Klaster",
                   conditionalPanel(
                     condition = "!output.fileUploaded",
                     div(class = "upload-prompt", "Unggah file terlebih dahulu untuk melihat analisis.")
                   ),
                   conditionalPanel(
                     condition = "output.fileUploaded",
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
                         # --- MODIFICATION: Used tabsetPanel for better organization ---
                         tabsetPanel(
                           tabPanel("Ringkasan", verbatimTextOutput("cluster_summary")),
                           tabPanel("Pusat Klaster (K-Means)", DT::dataTableOutput("cluster_centers_table")),
                           tabPanel("Jumlah Anggota per Klaster", DT::dataTableOutput("cluster_membership_table"))
                         )
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Penentuan Jumlah Klaster Optimal (Untuk K-Means)", status = "warning", solidHeader = TRUE, width = 12,
                         p("Gunakan plot ini untuk membantu menentukan nilai 'k' terbaik. Plot Elbow menunjukkan 'siku' di mana penambahan klaster tidak lagi memberikan banyak informasi. Plot Silhouette menunjukkan seberapa baik setiap objek berada dalam klasternya."),
                         actionButton("calculate_k", "Tampilkan Plot Elbow & Silhouette"),
                         hr(),
                         fluidRow(
                           column(6, plotOutput("elbow_plot")),
                           column(6, plotOutput("silhouette_plot"))
                         )
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Visualisasi Klaster", status = "success", solidHeader = TRUE, width = 12,
                         plotOutput("cluster_plot", height = "600px")
                       )
                     )
                   )
          ),
          tabPanel("🔍 MANOVA",
                   conditionalPanel(
                     condition = "!output.fileUploaded",
                     div(class = "upload-prompt", "Unggah file terlebih dahulu untuk melihat analisis.")
                   ),
                   conditionalPanel(
                     condition = "output.fileUploaded",
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
                   )
          ),
          tabPanel("📥 Unduh Hasil",
                   br(),
                   conditionalPanel(
                     condition = "!output.fileUploaded",
                     div(class = "upload-prompt", "Unggah file terlebih dahulu untuk mengunduh laporan.")
                   ),
                   conditionalPanel(
                     condition = "output.fileUploaded",
                     div(class = "download-section",
                         h4("📊 File Visualisasi Data Anda Sudah Siap!"),
                         p("Unduh hasil visualisasi dan analisis data anda dalam format PDF."),
                         downloadButton("download_custom", "📈 Unduh Laporan Visualisasi", class = "btn-download")
                     )
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
  
  # Check if a file has been uploaded
  output$fileUploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
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
  
  theme_color <- reactiveVal("#000000")
  legend_color <- reactiveVal("#000000")
  
  observeEvent(input$theme_default, {
    theme_color("#000000")  # Hitam untuk tema default
    legend_color("#000000")
  })
  
  observeEvent(input$theme_light, {
    theme_color("#303030")  # Abu untuk mode light
    legend_color("#303030")
  })
  
  observeEvent(input$theme_dark, {
    theme_color("#ffffff")  # Putih untuk mode dark
    legend_color("#ffffff")
  })
  
  # Data preview
  output$preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(head(values$data, 50), rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # Data overview
  # Ringkasan Dataset
  output$summary <- DT::renderDataTable({
    req(values$data)
    data <- values$data
    
    summary_list <- lapply(data, summary)
    
    summary_matrix <- do.call(cbind, summary_list)
    
    transposed_summary <- t(summary_matrix)
    
    summary_df <- as.data.frame(transposed_summary)
    
    summary_df$Variabel <- rownames(summary_df)
    
    summary_df <- summary_df[, c(setdiff(names(summary_df), "Variabel"))]
    
    DT::datatable(summary_df,
                  options = list(scrollX = TRUE, pageLength = 15))
  })
  
  # Struktur Dataset
  output$structure <- DT::renderDataTable({
    req(values$data)
    
    data <- values$data
    
    structure_df <- data.frame(
      Tipe = sapply(data, function(x) class(x)[1]),
      'Jumlah Unik' = sapply(data, function(x) length(unique(x))),
      Preview = sapply(data, function(x) {
        x_non_na <- na.omit(x)
        if (is.numeric(x)) {
          paste0(head(unique(x_non_na), 5), collapse = ", ")
        } else if (is.character(x) || is.factor(x)) {
          paste0(head(unique(x_non_na), 5), collapse = ", ")
        } else {
          "—"
        }
      }),
      row.names = names(data),
      check.names = FALSE
    )
    
    DT::datatable(structure_df,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Dataset Lengkap
  output$fulldata <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data, rownames = FALSE, options = list(scrollX = TRUE))
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
  
  output$cluster_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("selected_cluster_vars", "Pilih Variabel:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars)
  })
  
  output$manova_group_var <- renderUI({
    req(values$data)
    factor_vars <- names(values$data)[sapply(values$data, function(x) is.factor(x) || is.character(x))]
    selectInput("manova_group", "Variabel Independen:", multiple = TRUE, choices = factor_vars)
  })
  
  output$manova_dep_vars <- renderUI({
    req(values$data)
    numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    selectInput("manova_dependent", "Variabel Dependen:",
                choices = numeric_vars, multiple = TRUE, selected = numeric_vars[1:min(3, length(numeric_vars))])
  })
  
  # Descriptive Statistics
  output$descriptive_stats <- DT::renderDataTable({
    req(values$data, input$selected_desc_vars)
    
    desc <- psych::describe(values$data[, input$selected_desc_vars, drop = FALSE])
    
    selected_metrics <- c("n", "mean", "sd", "median", "trimmed", "mad", 
                          "min", "max", "range", "skew", "kurtosis", "se")
    
    desc_clean <- desc[, selected_metrics]
    rownames(desc_clean) <- rownames(desc)
    
    DT::datatable(desc_clean, 
                  options = list(scrollX = TRUE, pageLength = 15))
  })
  
  output$distribution_plots <- renderPlotly({
    req(values$data, input$selected_desc_vars)
    data_subset <- values$data[, input$selected_desc_vars, drop = FALSE]
    
    plots <- lapply(names(data_subset), function(var) {
      plot_ly(data_subset, x = ~get(var), type = "histogram", name = var) %>%
        layout(
          xaxis = list(title = var, tickfont = list(color = theme_color()), titlefont = list(color = theme_color())),
          yaxis = list(title = "Frekuensi", tickfont = list(color = theme_color()), titlefont = list(color = theme_color())),
          legend = list(font = list(color = legend_color())),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)')
    })
    
    subplot(plots, nrows = ceiling(length(plots)/2))
  })
  
  # Correlation Analysis
  output$correlation_matrix <- DT::renderDataTable({
    req(values$data, input$selected_corr_vars)
    
    corr_matrix <- cor(values$data[, input$selected_corr_vars], method = input$corr_method, use = "complete.obs")
    
    corr_df <- round(corr_matrix, 3)
    
    corr_df <- as.data.frame(corr_df)
    corr_df <- cbind(Variabel = rownames(corr_df), corr_df)
    rownames(corr_df) <- NULL
    
    DT::datatable(corr_df,
                  rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$correlation_plot <- renderPlot({
    req(values$data, input$selected_corr_vars)
    
    corr_matrix <- cor(values$data[, input$selected_corr_vars], method = input$corr_method, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
             tl.cex = 0.8, tl.col = "black", tl.srt = 45, bg = "transparent")
  }, bg = "transparent")
  
  # PCA Analysis
  # Tabel 1: Eigenvalues & Variansi
  output$pca_eigenvalues <- DT::renderDataTable({
    req(values$data, input$selected_pca_vars)
    pca_data <- na.omit(values$data[, input$selected_pca_vars])
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    
    eig_df <- as.data.frame(pca_result$eig)
    eig_df <- round(eig_df, 3)
    
    eig_df$Komponen <- paste0("PC", seq_len(nrow(eig_df)))
    eig_df <- eig_df[, c("Komponen", colnames(eig_df)[1:3])]
    rownames(eig_df) <- NULL  # ini penting agar tidak muncul kolom 'comp 1' dll
    
    colnames(eig_df)[2:4] <- c("Eigenvalue", "Proporsi Variansi (%)", "Kumulatif Variansi (%)")
    
    DT::datatable(eig_df,
                  rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  # Tabel 2: Loadings / Koordinat Variabel
  output$pca_varloadings <- DT::renderDataTable({
    req(values$data, input$selected_pca_vars)
    pca_data <- na.omit(values$data[, input$selected_pca_vars])
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    
    df <- data.frame(
      round(pca_result$var$coord, 3),
      round(pca_result$var$cos2, 3),
      round(pca_result$var$contrib, 3)
    )
    
    colnames(df) <- c(
      paste0("Koordinat PC", 1:ncol(pca_result$var$coord)),
      paste0("Cosinus Kuadrat PC", 1:ncol(pca_result$var$cos2)),
      paste0("Kontribusi PC", 1:ncol(pca_result$var$contrib))
    )
    
    df_t <- as.data.frame(t(df))
    colnames(df_t) <- rownames(pca_result$var$coord)
    df_t <- cbind(Metrik = rownames(df_t), df_t)
    rownames(df_t) <- NULL
    
    DT::datatable(df_t,
                  options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Tabel 3: Skor Individu (bisa dikurangi jumlah barisnya kalau banyak)
  output$pca_indiv_scores <- DT::renderDataTable({
    req(values$data, input$selected_pca_vars)
    pca_data <- na.omit(values$data[, input$selected_pca_vars])
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    
    df <- data.frame(
      `Jarak` = round(pca_result$ind$dist, 3),
      round(pca_result$ind$coord, 3),
      round(pca_result$ind$cos2, 3),
      round(pca_result$ind$contrib, 3)
    )
    
    colnames(df) <- c(
      "Jarak",
      paste0("PC", 1:ncol(pca_result$ind$coord)),
      paste0("Cosinus Kuadrat PC", 1:ncol(pca_result$ind$cos2)),
      paste0("Kontribusi PC", 1:ncol(pca_result$ind$contrib))
    )
    
    df_t <- as.data.frame(t(df))
    colnames(df_t) <- rownames(pca_result$ind$coord)
    df_t <- cbind(Metrik = rownames(df_t), df_t)
    rownames(df_t) <- NULL
    
    DT::datatable(df_t,
                  rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$scree_plot <- renderPlot({
    req(values$data, input$selected_pca_vars)
    pca_data <- values$data[, input$selected_pca_vars]
    pca_data <- pca_data[complete.cases(pca_data), ]
    
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    p <- fviz_eig(pca_result, addlabels = TRUE)
    p + theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      # Gunakan theme_color() untuk teks agar dinamis dengan tema
      text = element_text(color = theme_color()),
      axis.text = element_text(color = theme_color()),
      axis.title = element_text(color = theme_color()),
      plot.title = element_text(color = theme_color(), hjust = 0.5),
      
      legend.title = element_text(color = theme_color()),
      legend.text = element_text(color = theme_color())
    )
  }, bg = "transparent")
  
  output$pca_biplot <- renderPlot({
    req(values$data, input$selected_pca_vars)
    pca_data <- values$data[, input$selected_pca_vars]
    pca_data <- pca_data[complete.cases(pca_data), ]
    
    pca_result <- PCA(pca_data, scale.unit = input$pca_scale, graph = FALSE)
    fviz_pca_biplot(pca_result, repel = TRUE) + 
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        # Gunakan theme_color() untuk teks agar dinamis dengan tema
        text = element_text(color = theme_color()),
        axis.text = element_text(color = theme_color()),
        axis.title = element_text(color = theme_color()),
        plot.title = element_text(color = theme_color(), hjust = 0.5),
        
        legend.title = element_text(color = theme_color()),
        legend.text = element_text(color = theme_color())
      )
  }, bg = "transparent")
  
  # Cluster Analysis
  # --- Reactive expression for preparing cluster data ---
  cluster_data_prepared <- reactive({
    req(values$data, input$selected_cluster_vars)
    df <- values$data[, input$selected_cluster_vars, drop = FALSE]
    df <- df[complete.cases(df), ]
    if(input$cluster_scale) {
      df <- scale(df)
    }
    return(as.data.frame(df))
  })
  
  # --- Reactive expression for the main clustering result ---
  cluster_result_object <- reactive({
    cluster_data <- cluster_data_prepared()
    
    if (input$cluster_method == "kmeans") {
      req(input$k_clusters)
      kmeans(cluster_data, centers = input$k_clusters, nstart = 25)
    } else { # hclust
      req(input$linkage)
      dist_matrix <- dist(cluster_data)
      hclust(dist_matrix, method = input$linkage)
    }
  })
  
  optimal_k_plots <- eventReactive(input$calculate_k, {
    data_for_k <- cluster_data_prepared()
    req(nrow(data_for_k) > 1) # Ensure data is not empty
    
    # Show a notification
    showNotification("Menghitung plot Elbow dan Silhouette...", type = "message", duration = 3)
    
    # Generate plots
    p1 <- fviz_nbclust(data_for_k, kmeans, method = "wss") +
      labs(title = "Elbow Method")
    
    p2 <- fviz_nbclust(data_for_k, kmeans, method = "silhouette") +
      labs(title = "Silhouette Method")
    
    return(list(elbow = p1, silhouette = p2))
  })
  
  # --- Render Elbow Plot ---
  output$elbow_plot <- renderPlot({
    req(optimal_k_plots())
    optimal_k_plots()$elbow + theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      text = element_text(color = theme_color()),
      axis.text = element_text(color = theme_color()),
      axis.title = element_text(color = theme_color()),
      plot.title = element_text(color = theme_color(), hjust = 0.5)
    )
  }, bg = "transparent")
  
  # --- Render Silhouette Plot ---
  output$silhouette_plot <- renderPlot({
    req(optimal_k_plots())
    optimal_k_plots()$silhouette + theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      text = element_text(color = theme_color()),
      axis.text = element_text(color = theme_color()),
      axis.title = element_text(color = theme_color()),
      plot.title = element_text(color = theme_color(), hjust = 0.5)
    )
  }, bg = "transparent")
  
  output$cluster_summary <- renderPrint({
    req(cluster_result_object())
    print(cluster_result_object())
  })
  
  output$cluster_centers_table <- DT::renderDataTable({
    req(input$cluster_method == "kmeans", cluster_result_object())
    centers <- as.data.frame(round(cluster_result_object()$centers, 3))
    centers <- cbind(Klaster = rownames(centers), centers)
    rownames(centers) <- NULL
    DT::datatable(centers, rownames = FALSE, options = list(scrollX = TRUE))
  })
  
  output$cluster_membership_table <- DT::renderDataTable({
    req(cluster_result_object())
    
    if (input$cluster_method == "kmeans") {
      members <- as.data.frame(table(cluster_result_object()$cluster))
    } else { # hclust
      req(input$k_clusters) # Use k_clusters to cut the tree
      clusters <- cutree(cluster_result_object(), k = input$k_clusters)
      members <- as.data.frame(table(clusters))
    }
    
    colnames(members) <- c("Klaster", "Jumlah Anggota")
    DT::datatable(members, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  output$cluster_plot <- renderPlot({
    cluster_data <- cluster_data_prepared()
    
    p <- if(input$cluster_method == "kmeans") {
      req(cluster_result_object())
      fviz_cluster(cluster_result_object(), data = cluster_data,
                   ggtheme = theme_minimal()) +
        labs(title = "K-Means Clustering Visualization")
      
    } else { # hclust
      req(cluster_result_object())
      fviz_dend(cluster_result_object(), k = input$k_clusters, cex = 0.7,
                k_colors = "jco", ggtheme = theme_minimal()) +
        labs(title = "Hierarchical Clustering Dendrogram")
    }
    
    p + theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      text = element_text(color = theme_color()),
      axis.text = element_text(color = theme_color()),
      axis.title = element_text(color = theme_color()),
      plot.title = element_text(color = theme_color(), hjust = 0.5),
      legend.title = element_text(color = theme_color()),
      legend.text = element_text(color = theme_color())
    )
  }, bg = "transparent")
  
  # MANOVA
  output$manova_results <- renderPrint({
    req(values$data, input$manova_group, input$manova_dependent)
    all_vars <- c(input$manova_group, input$manova_dependent)
    manova_data <- values$data[, all_vars]
    manova_data <- manova_data[complete.cases(manova_data), ]
    
    dep_vars <- paste("cbind(", paste(input$manova_dependent, collapse = ", "), ")")
    indep_vars <- paste(input$manova_group, collapse = " + ")
    formula_str <- paste(dep_vars, "~", indep_vars)
    manova_result <- manova(as.formula(formula_str), data = manova_data)
    summary(manova_result, test = "Pillai") # Anda bisa memilih test lain seperti "Wilks", "Hotelling-Lawley", atau "Roy"
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
  
  # Download handler for comprehensive report
  output$download_custom <- downloadHandler(
    filename = function() {
      paste("Comprehensive_Analysis_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Show a notification while the report is being generated
      showNotification("Generating your PDF report... Please wait.",
                       duration = 10,
                       type = "message")
      
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Create a list of parameters to pass to the Rmd file
      params <- list(
        data = values$data,
        desc_vars = input$selected_desc_vars,
        corr_vars = input$selected_corr_vars,
        corr_method = input$corr_method,
        pca_vars = input$selected_pca_vars,
        pca_scale = input$pca_scale,
        cluster_vars = input$selected_cluster_vars,
        cluster_method = input$cluster_method,
        cluster_scale = input$cluster_scale,
        k_clusters = input$k_clusters,
        linkage = input$linkage,
        manova_group = input$manova_group,
        manova_dependent = input$manova_dependent
      )
      
      # Define the R Markdown content as a string
      rmd_content <- '
---
title: "Comprehensive Data Analysis Report"
subtitle: "Generated from MultiBlitz App"
date: "`r Sys.Date()`"
output: 
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
geometry: landscape, margin=0.8in
params:
  data: NULL
  desc_vars: NULL
  corr_vars: NULL
  corr_method: "pearson"
  pca_vars: NULL
  pca_scale: TRUE
  cluster_vars: NULL
  cluster_method: "kmeans"
  cluster_scale: TRUE
  k_clusters: 3
  linkage: "complete"
  manova_group: NULL
  manova_dependent: NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 6)
library(ggplot2)
library(dplyr)
library(knitr)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(cluster)
library(psych)
library(gridExtra)

# Helper to check for valid data and variables
is_valid <- function(data, vars) {
  !is.null(data) && !is.null(vars) && length(vars) > 0
}


# Dataset Overview

```{r dataset-overview}
if (!is.null(params$data)) {
  knitr::kable(head(params$data, 10))
}
```

# Statistical Summary

```{r stat-summary}
if (!is.null(params$data)) {
  numeric_vars <- names(params$data)[sapply(params$data, is.numeric)]
  if(length(numeric_vars) > 0) {
    summary_stats <- describe(params$data[, numeric_vars, drop = FALSE])
    knitr::kable(summary_stats[, c("n", "mean", "sd", "min", "max")], digits = 2)
  } else {
    cat("No numeric variables to summarize.")
  }
}
```

# Distribution Plot

```{r dist-plot}
if (is_valid(params$data, params$desc_vars)) {
  plots <- lapply(params$desc_vars, function(var) {
    ggplot(params$data, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = paste("Distribution of", var), x = var, y = "Count") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(size = 12, hjust = 0.5))
  })
  do.call(grid.arrange, c(plots, ncol = 2))
} else {
  cat("No variables selected for distribution plots.")
}
```

# Correlation Analysis

```{r correlation-matrix}
if (is_valid(params$data, params$corr_vars) && length(params$corr_vars) >= 2) {
  corr_matrix <- cor(params$data[, params$corr_vars], method = params$corr_method, use = "complete.obs")
  knitr::kable(round(corr_matrix, 3))
}
```

```{r correlation-plot}
if (is_valid(params$data, params$corr_vars) && length(params$corr_vars) >= 2) {
  corr_matrix <- cor(params$data[, params$corr_vars], method = params$corr_method, use = "complete.obs")
  knitr::kable(round(corr_matrix, 3))
}
```

# PCA Analysis
```{r PCA-summary}
if (is_valid(params$data, params$pca_vars) && length(params$pca_vars) >= 2) {
  pca_data <- params$data[, params$pca_vars]
  pca_data <- pca_data[complete.cases(pca_data), ]
  pca_result <- PCA(pca_data, scale.unit = params$pca_scale, graph = FALSE)
  
  # Eigenvalues Table
  eig_df <- as.data.frame(pca_result$eig)
  eig_df <- round(eig_df, 3)
  eig_df$Komponen <- paste0("PC", seq_len(nrow(eig_df)))
  eig_df <- eig_df[, c("Komponen", "eigenvalue", "percentage of variance", "cumulative percentage of variance")]
  colnames(eig_df) <- c("Komponen", "Eigenvalue", "Proporsi Variansi (%)", "Kumulatif Variansi (%)")
  knitr::kable(eig_df, caption = "Eigenvalues and Variance Explained")
  
  # Variable Loadings Table
  var_df <- data.frame(
    round(pca_result$var$coord, 3),
    round(pca_result$var$cos2, 3),
    round(pca_result$var$contrib, 3)
  )
  colnames(var_df) <- c(
    paste0("Koordinat PC", 1:ncol(pca_result$var$coord)),
    paste0("Cos^2 PC", 1:ncol(pca_result$var$cos2)),
    paste0("Kontribusi PC", 1:ncol(pca_result$var$contrib))
  )
  var_df <- cbind(Variabel = rownames(var_df), var_df)
  knitr::kable(var_df, caption = "Variable Loadings, Cosinus Squared, and Contributions")
  
  # Individual Scores Table
  ind_df <- data.frame(
    Jarak = round(pca_result$ind$dist, 3),
    round(pca_result$ind$coord, 3),
    round(pca_result$ind$cos2, 3),
    round(pca_result$ind$contrib, 3)
  )
  colnames(ind_df) <- c(
    "Jarak",
    paste0("PC", 1:ncol(pca_result$ind$coord)),
    paste0("Cos^2 PC", 1:ncol(pca_result$ind$cos2)),
    paste0("Kontribusi PC", 1:ncol(pca_result$ind$contrib))
  )
  ind_df <- ind_df[1:min(10, nrow(ind_df)), ] # Limit to first 10 rows for brevity
  knitr::kable(ind_df, caption = "Individual Scores (First 10 Observations)")
} else {
  cat("At least two variables are needed for PCA.")
}
```

```{r PCA-scree}
if (is_valid(params$data, params$pca_vars) && length(params$pca_vars) >= 2) {
  pca_data <- params$data[, params$pca_vars]
  pca_data <- pca_data[complete.cases(pca_data), ]
  pca_result <- PCA(pca_data, scale.unit = params$pca_scale, graph = FALSE)
  print(fviz_eig(pca_result, addlabels = TRUE))
} else {
  cat("At least two variables are needed for PCA.")
}
```

```{r PCA-biplot}
if (is_valid(params$data, params$pca_vars) && length(params$pca_vars) >= 2) {
  pca_data <- params$data[, params$pca_vars]
  pca_data <- pca_data[complete.cases(pca_data), ]
  pca_result <- PCA(pca_data, scale.unit = params$pca_scale, graph = FALSE)
  print(fviz_pca_biplot(pca_result, repel = TRUE))
}
```


# Cluster Analysis
```{r silhouette-elbow}
if (is_valid(params$data, params$cluster_vars) && params$cluster_method == "kmeans") {
  cluster_data <- params$data[, params$cluster_vars, drop = FALSE]
  cluster_data <- cluster_data[complete.cases(cluster_data), ]
  if (params$cluster_scale) cluster_data <- scale(cluster_data)
  
  p1 <- fviz_nbclust(cluster_data, kmeans, method = "wss") + labs(title = "Elbow Method")
  p2 <- fviz_nbclust(cluster_data, kmeans, method = "silhouette") + labs(title = "Silhouette Method")
  
  grid.arrange(p1, p2, ncol = 2)
}
```


```{r cluster}
if (is_valid(params$data, params$cluster_vars) && length(params$cluster_vars) >= 2) {
  cluster_data <- params$data[, params$cluster_vars]
  cluster_data <- cluster_data[complete.cases(cluster_data), ]
  if (params$cluster_scale) cluster_data <- scale(cluster_data)
  
  if (params$cluster_method == "kmeans") {
    set.seed(123) # for reproducibility
    cluster_result <- kmeans(cluster_data, centers = params$k_clusters, nstart = 25)
    print(fviz_cluster(cluster_result, data = cluster_data, main = "K-Means Cluster Plot"))
  } else { # hclust
    dist_matrix <- dist(cluster_data)
    cluster_result <- hclust(dist_matrix, method = params$linkage)
    print(fviz_dend(cluster_result, k = params$k_clusters, cex = 0.5, main = "Hierarchical Clustering Dendrogram"))
  }
} else {
  cat("At least two variables are needed for Cluster Analysis.")
}
```

# MANOVA

```{r manova}
if (is_valid(params$data, params$manova_group) && is_valid(params$data, params$manova_dependent)) {
  all_vars <- c(params$manova_group, params$manova_dependent)
  manova_data <- params$data[, all_vars]
  manova_data <- manova_data[complete.cases(manova_data), ]
  
  dep_vars_str <- paste("cbind(", paste(params$manova_dependent, collapse = ", "), ")")
  indep_vars_str <- paste(params$manova_group, collapse = " + ")
  
  formula_str <- paste(dep_vars_str, "~", indep_vars_str)
  
  manova_result <- manova(as.formula(formula_str), data = manova_data)
  
  # Capture the summary to print it
  summary_output <- summary(manova_result, test = "Pillai")
  print(summary_output)
  
} else {
  cat("Independent and Dependent variables must be selected for MANOVA.")
}
```
---

*Custom report generated from Multiblitz*
'
      
      writeLines(rmd_content, temp_rmd)
      rmarkdown::render(temp_rmd, output_file = file, params = params, 
                        envir = new.env(parent = globalenv()), quiet = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)