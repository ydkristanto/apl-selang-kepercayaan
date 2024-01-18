# Memanggil paket ----
library(shiny)
library(ggplot2)

# Mendefinisikan UI ----
ui <- fluidPage(
  title = "Demonstrasi Cakupan Selang Kepercayaan -- Aplikasi Shiny",
  navbarPage("Cakupan Selang Kepercayaan",
             position = "static-top",
             ## Tab proporsi ----
             tabPanel("Proporsi",
                      sidebarPanel(
                        wellPanel(
                          ### Memilih proporsi populasi ----
                          sliderInput("p_prop", "Proporsi populasi:",
                                      0.5, min = 0, max = 1, step = 0.01)
                        ),
                        wellPanel(
                          ### Memilih tingkat kepercayaan SK proporsi ----
                          selectInput("tingkat_keper_prop",
                                      "Tingkat kepercayaan:",
                                      choices = c("90%" = "0.90",
                                                  "95%" = "0.95",
                                                  "99%" = "0.99"),
                                      selected = "0.95")
                        ),
                        wellPanel(
                          ### Memilih ukuran sampel proporsi ----
                          sliderInput("n_prop", "Ukuran sampel:",
                                      30, min = 10, max = 100, step = 5),
                          ### Memilih banyak sampel proporsi ----
                          sliderInput("k_prop", "Banyak sampel:",
                                      20, min = 10, max = 500, step = 10)
                        )
                      ),
                      mainPanel(
                        plotOutput("plot_cakupan_prop",
                                   height = "450px"),
                        textOutput("teks_cakupan_prop")
                      )
             ),
             ## Tab rerata ----
             tabPanel("Rerata",
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            ### Memilih tingkat kepercayaan SK rerata ----
                            radioButtons("sigma_rrt",
                                         "Sigma diketahui:",
                                         choices = c("Ya", "Tidak"),
                                         selected = "Tidak",
                                         inline = TRUE)
                            ),
                          wellPanel(
                            selectInput("tingkat_keper_rrt",
                                        "Tingkat kepercayaan:",
                                        choices = c("90%" = "0.9",
                                                    "95%" = "0.95",
                                                    "99%" = "0.99"),
                                        selected = "0.95")
                            ),
                          wellPanel(
                            ### Memilih ukuran sampel ----
                            sliderInput("n_rrt",
                                        "Ukuran sampel:", 
                                        value = 30,
                                        min = 2,
                                        max = 100),
                            hr(),
                            ### Menentukan banyak sampel ----
                            sliderInput("k_rrt",
                                        "Banyaknya sampel:",
                                        value = 20,
                                        min = 10,
                                        max = 500,
                                        step = 10)
                          )
                        ),
                        mainPanel(
                          ### Plot output rerata ----
                          plotOutput("plot_cakupan_rrt",
                                     height = "450px")
                          )
                        )
                      ),
             ## Tab informasi ----
             tabPanel("Informasi",
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            div(h4("Deskripsi",
                             style = "font-size: inherit;
                             font-weight: bold")),
                             div(p("Aplikasi Shiny ini digunakan untuk mendemonstrasikan Teorema Limit Pusat untuk distribusi sampling proporsi dan rerata."))
                             ),
                           wellPanel(
                             div(h4("Kode sumber",
                             style = "font-size: inherit;
                             font-weight: bold")),
                             div(p("Kode sumber aplikasi ini tersedia di repositori", a("Github.", href = "https://github.com/ydkristanto/apl-tlp", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-tlp/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-tlp/pulls", target = "_blank"), "di repositori tersebut."))
                             ),
                           wellPanel(
                             div(h4("Lisensi",
                             style = "font-size: inherit;
                             font-weight: bold")),
                             div(p("Lisensi MIT"),
                                 p("Copyright (c) 2024 Yosep Dwi Kristanto"))
                             )
                           ),
                         mainPanel(
                           div(h3("Aplikasi Shiny Teorema Limit Pusat")),
                           div(p("Tujuan aplikasi interaktif ini adalah untuk mendemonstrasikan Teorema Limit Pusat untuk distribusi sampling proporsi dan rerata satu populasi. Beberapa ide penting statistik ditunjukkan oleh aplikasi ini. Ide-ide penting tersebut antara lain adalah sebagai berikut."), align = "justify"),
                           div(tags$ul(tags$li("Jika ukuran sampelnya cukup besar, distribusi sampling proporsinya mendekati normal."),
                                       tags$li("Distribusi sampling proporsi tersebut memiliki rerata sama dengan proporsi populasinya dan simpangan bakunya sama dengan akar kuadrat dari hasil kali antara proporsi populasi dan satu dikurangi proporsi tersebut kemudian dibagi dengan ukuran sampel."),
                                       tags$li("Jika ukuran sampelnya cukup besar, distribusi sampling reratanya mendekati normal."),
                                       tags$li("Untuk sampel yang berukuran kecil, distribusi sampling reratanya mendekati normal jika populasi dari sampel tersebut berdistribusi normal."),
                                       tags$li("Distribusi sampling rerata tersebut memiliki rerata yang sama dengan rerata populasinya dan simpangan baku yang sama dengan simpangan baku populasi dibagi dengan akar kuadrat ukuran sampel.")), align = "justify"),
                           hr(),
                           div(p("Aplikasi interaktif ini dikembangkan dengan menggunakan bahasa pemrogram", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah."), align = "justify"),
                           div(p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta. Aplikasi ini merupakan modifikasi dari aplikasi-aplikasi interaktif", a("ShinyEd", href = "https://github.com/ShinyEd/intro-stats/", target = "_blank"), "yang dikembangkan oleh Mine Çetinkaya-Rundel dkk."), align = "justify"),
                  width = 6)
                  )
                  )
              )
     )

# Fungsi peladen ----
seed = as.numeric(Sys.time())
server <- function(input, output) {
  ## Fungsi untuk proporsi ----
  ## Fungsi untuk membuat data dan menentukan selang kepercayaan
  membuat_data <- function(p, n_prop, k_prop, tk_prop) {
    set.seed(seed)
    set_sampel <- matrix(rbinom(n_prop * k_prop, size = 1, prob = p),
                         ncol = k_prop)
    data_sk <- lapply(1:k_prop, function(i) {
      sampel <- set_sampel[, i]
      prop_test <- prop.test(sum(sampel), length(sampel),
                             conf.level = as.numeric(tk_prop))
      sk <- prop_test$conf.int
      rerata <- mean(sampel)
      mencakup_p <- p >= sk[1] && p <= sk[2]
      data.frame(x = i, xend = i, y = sk[1], yend = sk[2],
                 mencakup_p = mencakup_p, rerata = rerata)
    })
    return(data_sk)
  }
  
  # Plot cakupan selang kepercayaan
  output$plot_cakupan_prop <- renderPlot({
    data_sk <- membuat_data(input$p_prop, input$n_prop,
                            input$k_prop, input$tingkat_keper_prop)
    
    ggplot() +
      geom_segment(data = do.call(rbind, data_sk),
                   aes(x = x, xend = xend, y = y, yend = yend,
                       color = factor(mencakup_p)),
                   linewidth = 1,
                   alpha = .6) +
      geom_point(data = do.call(rbind, data_sk),
                 aes(x = x, y = rerata,
                     color = factor(mencakup_p)),
                 size = 2) +
      geom_hline(yintercept = input$p_prop, linetype = "dashed",
                 color = "black", linewidth = 1) +
      scale_color_manual(values = c("FALSE" = "#d95f02",
                                    "TRUE" = "#1b9e77"),
                         name = "Mencakup p?",
                         labels = c("FALSE" = "Tidak",
                                    "TRUE" = "Ya")) +
      labs(title = "Cakupan selang kepercayaan proporsi",
           y = "Proporsi") +
      theme_bw(base_size = 16) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "bottom")
  })
  
  # Display coverage percentage
  output$teks_cakupan_prop <- renderText({
    n <- input$n_prop
    k <- input$k_prop
    tingkat_keper <- as.numeric(input$tingkat_keper_prop) * 100
    data_sk <- membuat_data(input$p_prop, input$n_prop,
                            input$k_prop, input$tingkat_keper_prop)
    persen_mencakup <- mean(sapply(data_sk,
                                   function(sk) sk$mencakup_p)) * 100
    paste("Gambar di atas memvisualisasikan ", k, " selang kepercayaan ", tingkat_keper, "% dari tiap-tiap sampel yang terpilih. Persentase selang kepercayaan yang mencakup proporsi populasi: ", round(persen_mencakup, 2), "%", sep = "")
  })
  ## Fungsi untuk rerata ----
  # Tetapkan parameter populasi
  mean_populasi <- 500
  sd_populasi <- 100
  
  # Fungsi untuk menghasilkan data dan menghitung selang kepercayaan
  hasilkan_data <- function(n, k, tk, sigma) {
    data_sk <- lapply(1:k, function(i) {
      sampel <- rnorm(n, mean = 500, sd = 100)
      rerata_sampel <- mean(sampel)
      if (sigma == "Ya") {
        selang_kepercayaan <- qnorm(c(0.5 - as.numeric(tk)/2,
                                      0.5 + as.numeric(tk)/2),
                                    mean = rerata_sampel,
                                    sd = 100 / sqrt(n))
      } else {
        selang_kepercayaan <- t.test(sampel,
                                     conf.level = as.numeric(tk))$conf.int
      }
      
      # Periksa apakah selang kepercayaan mencakup rerata populasi
      mencakup_rrt_populasi <- selang_kepercayaan[1] <= 500 &&
        selang_kepercayaan[2] >= 500
      
      data.frame(x = i, xend = i, y = selang_kepercayaan[1],
                 yend = selang_kepercayaan[2], rerata = rerata_sampel,
                 mencakup_rrt_populasi = mencakup_rrt_populasi)
    })
    return(data_sk)
  }
  rep_hasilkan_data <- repeatable(hasilkan_data)
  
  ### Plot selang kepercayaan rerata ----
  output$plot_cakupan_rrt <- renderPlot({
    data_sk <- rep_hasilkan_data(input$n_rrt, input$k_rrt,
                                 input$tingkat_keper_rrt,
                                 input$sigma_rrt)
    
    ggplot() +
      geom_segment(data = do.call(rbind, data_sk),
                   aes(x = x, xend = xend, y = y, yend = yend,
                       color = factor(mencakup_rrt_populasi)),
                   size = 1,
                   alpha = .6) +
      geom_point(data = do.call(rbind, data_sk),
                 aes(x = x, y = rerata,
                     color = factor(mencakup_rrt_populasi)),
                 size = 2) +
      geom_hline(yintercept = 500, linetype = "dashed",
                 linewidth = 1) +
      scale_color_manual(name = "Mencakup mu?",
                         values = c("TRUE" = "#1b9e77",
                                    "FALSE" = "#d95f02"),
                         labels = c("TRUE" = "Ya",
                                    "FALSE" = "Tidak")) +
      labs(title = "Cakupan selang kepercayaan rerata",
           x = "Nomor Sampel", y = "Nilai") +
      theme_bw(base_size = 16) +
      theme(legend.position = "bottom",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  })
}

# Membuat objek aplikasi Shiny ----
shinyApp(ui = ui, server = server)