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
                        ### Panel utama ---
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
                            ### Memilih distribusi populasi ----
                            radioButtons("dist_pop_rrt",
                                         "Distribusi populasi:",
                                         choices = c("Normal" = "rnorm",
                                                     "Seragam" = "runif",
                                                     "Condong ke kanan" = "rlnorm",
                                                     "Condong ke kiri" = "rbeta",
                                                     "Puncak ganda" = "rnorm2"),
                                         selected = "rnorm"),
                            ### Memilih kondisi statistik ----
                            radioButtons("sigma_rrt",
                                         "Sigma diketahui:",
                                         choices = c("Ya", "Tidak"),
                                         selected = "Tidak",
                                         inline = TRUE)
                            ),
                          wellPanel(
                            ### Memilih tingkat kepercayaan SK rerata ----
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
                                     height = "450px"),
                          textOutput("teks_cakupan_rrt")
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
                             div(p("Aplikasi Shiny ini digunakan untuk mendemonstrasikan cakupan selang kepercayaan proporsi dan rerata terhadap parameter populasinya."))
                             ),
                           wellPanel(
                             div(h4("Kode sumber",
                             style = "font-size: inherit;
                             font-weight: bold")),
                             div(p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-selang-kepercayaan", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-selang-kepercayaan/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-selang-kepercayaan/pulls", target = "_blank"), "di repositori tersebut."))
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
                           div(h3("Aplikasi Shiny Selang Kepercayaan")),
                           div(p("Aplikasi interaktif ini dimaksudkan untuk mendemonstrasikan selang-selang kepercayaan proporsi dan rerata yang mencakup parameter populasinya. Beberapa ide penting statistik yang ditunjukkan oleh aplikasi ini antara lain sebagai berikut."), align = "justify"),
                           div(tags$ul(tags$li("Semakin besar tingkat kepercayaannya, semakin besar persentase selang kepercayaan yang mencakup parameter populasinya."),
                                       tags$li("Semakin besar tingkat kepercayaannya, pias galatnya juga semakin besar. Hal ini ditunjukkan dengan semakin panjangnya selang kepercayaannya."),
                                       tags$li("Untuk rerata, ketika ukuran sampelnya kecil, metode yang menganggap sigma (simpangan baku populasi) tidak diketahui cenderung menghasilkan persentase pencakupan rerata populasi yang lebih tinggi.")), align = "justify"),
                           hr(),
                           div(p("Aplikasi interaktif ini dikembangkan dengan menggunakan bahasa pemrogram", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah."), align = "justify"),
                           div(p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta."), align = "justify"),
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
  rep_membuat_data <- repeatable(membuat_data)
  
  # Plot cakupan selang kepercayaan
  output$plot_cakupan_prop <- renderPlot({
    data_sk <- rep_membuat_data(input$p_prop, input$n_prop,
                            input$k_prop, input$tingkat_keper_prop)
    k <- input$k_prop
    alpha_sk <- function(x) {
      return(-3 / 1200500 * (x - 10)^2 + 1)
    }
    
    ggplot() +
      geom_segment(data = do.call(rbind, data_sk),
                   aes(x = x, xend = xend, y = y, yend = yend,
                       color = factor(mencakup_p)),
                   linewidth = 1,
                   alpha = alpha_sk(k)) +
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
  
  ### Teks informasi grafik ----
  output$teks_cakupan_prop <- renderText({
    n <- input$n_prop
    k <- input$k_prop
    tingkat_keper <- as.numeric(input$tingkat_keper_prop) * 100
    data_sk <- rep_membuat_data(input$p_prop, input$n_prop,
                            input$k_prop, input$tingkat_keper_prop)
    persen_mencakup <- mean(sapply(data_sk,
                                   function(sk) sk$mencakup_p)) * 100
    paste("Gambar di atas memvisualisasikan ", k, " selang kepercayaan ", tingkat_keper, "% dari tiap-tiap sampel yang terpilih. Persentase selang kepercayaan yang mencakup proporsi populasi: ", round(persen_mencakup, 2), "%", sep = "")
  })
  ## Fungsi untuk rerata ----
  ### Fungsi hasilkan data ----
  rnorm2 <- function(n, mu1, mu2, sd){
    return(c(rnorm(n = round(2/5*n), mean = mu1, sd = sd),
             rnorm(n = n - round(2/5*n), mean = mu2, sd = sd)))
  }
  hasilkan_data <- function(dist, n, k, tk, sigma) {
    data_sk <- lapply(1:k, function(i) {
      if (dist == "rnorm") {
        sampel <- rnorm(n, mean = 500, sd = 100) 
        rrt_pop <- 500
        sd_pop <- 100
      } else if (dist == "rlnorm") {
        sampel <- rlnorm(n, meanlog = 6, sdlog = .3)
        rrt_pop <- exp(6 + (.3)^2 / 2)
        sd_pop <- sqrt((exp(.3^2) - 1) * exp(2 * 6 + .3^2))
      } else if (dist == "rbeta") {
        sampel <- rbeta(n, shape1 = 5, shape2 = 1.5) * 650
        rrt_pop <- 5 / (5 + 1.5) * 650
        sd_pop <- sqrt(5 * 1.5 / ((5 + 1.5)^2 * (5 + 1.5 + 1))) * 650
      } else if (dist == "runif") {
        sampel <- runif(n, min = 300, max = 700)
        rrt_pop <- 500
        sd_pop <- sqrt(1 / 12 * (700 - 300)^2)
      } else if (dist == "rnorm2") {
        sampel <- rnorm2(n, mu1 = 400, mu2 = 600, sd = 50)
        rrt_pop <- 2 / 5 * 400 + 3 / 5 * 600
        sd_pop <- sqrt(2/5 * (50^2 + (400 - 520)^2) + 
                         3 / 5 * (50^2 + (600 - 520)^2))
      }
      rerata_sampel <- mean(sampel)
      if (sigma == "Ya") {
        selang_kepercayaan <- qnorm(c(0.5 - as.numeric(tk)/2,
                                      0.5 + as.numeric(tk)/2),
                                    mean = rerata_sampel,
                                    sd = sd_pop / sqrt(n))
      } else {
        selang_kepercayaan <- t.test(sampel,
                                     conf.level = as.numeric(tk))$conf.int
      }
      
      # Periksa apakah selang kepercayaan mencakup rerata populasi
      mencakup_rrt_populasi <- selang_kepercayaan[1] <= rrt_pop &&
        selang_kepercayaan[2] >= rrt_pop
      
      data.frame(x = i, xend = i, y = selang_kepercayaan[1],
                 yend = selang_kepercayaan[2], rerata = rerata_sampel,
                 mencakup_rrt_populasi = mencakup_rrt_populasi)
    })
    return(data_sk)
  }
  rep_hasilkan_data <- repeatable(hasilkan_data)
  
  ### Plot selang kepercayaan rerata ----
  output$plot_cakupan_rrt <- renderPlot({
    data_sk <- rep_hasilkan_data(input$dist_pop_rrt, input$n_rrt, input$k_rrt,
                                 input$tingkat_keper_rrt,
                                 input$sigma_rrt)
    k <- input$k_rrt
    alpha_sk <- function(x) {
      return(-3 / 1200500 * (x - 10)^2 + 1)
    }
    if (input$dist_pop_rrt == "rnorm") {
      rrt_pop <- 500
    } else if (input$dist_pop_rrt == "rlnorm") {
      rrt_pop <- exp(6 + (.3)^2 / 2)
    } else if (input$dist_pop_rrt == "rbeta") {
      rrt_pop <- 5 / (5 + 1.5) * 650
    } else if (input$dist_pop_rrt == "runif") {
      rrt_pop <- 500
    } else if (input$dist_pop_rrt == "rnorm2") {
      rrt_pop <- 520
    }
    
    ggplot() +
      geom_segment(data = do.call(rbind, data_sk),
                   aes(x = x, xend = xend, y = y, yend = yend,
                       color = factor(mencakup_rrt_populasi)),
                   linewidth = 1,
                   alpha = alpha_sk(k)) +
      geom_point(data = do.call(rbind, data_sk),
                 aes(x = x, y = rerata,
                     color = factor(mencakup_rrt_populasi)),
                 size = 2) +
      geom_hline(yintercept = rrt_pop, linetype = "dashed",
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
  ### Teks informasi grafik ----
  output$teks_cakupan_rrt <- renderText({
    n <- input$n_rrt
    k <- input$k_rrt
    tingkat_keper <- as.numeric(input$tingkat_keper_rrt) * 100
    if (input$dist_pop_rrt == "rnorm") {
      dist_pop <- "normal"
      rrt_pop <- 500
      sd_pop <- 100
    } else if (input$dist_pop_rrt == "rlnorm") {
      dist_pop <- "condong ke kanan"
      rrt_pop <- exp(6 + (.3)^2 / 2)
      sd_pop <- sqrt((exp(.3^2) - 1) * exp(2 * 6 + .3^2))
    } else if (input$dist_pop_rrt == "rbeta") {
      dist_pop <- "condong ke kiri"
      rrt_pop <- 5 / (5 + 1.5) * 650
      sd_pop <- sqrt(5 * 1.5 / ((5 + 1.5)^2 * (5 + 1.5 + 1))) * 650
    } else if (input$dist_pop_rrt == "runif") {
      dist_pop <- "seragam"
      rrt_pop <- 500
      sd_pop <- sqrt(1 / 12 * (700 - 300)^2)
    } else if (input$dist_pop_rrt == "rnorm2") {
      dist_pop <- "puncak ganda"
      rrt_pop <- 2 / 5 * 400 + 3 / 5 * 600
      sd_pop <- sqrt(2/5 * (50^2 + (400 - 520)^2) + 
                       3 / 5 * (50^2 + (600 - 520)^2))
    }
    data_sk <- rep_hasilkan_data(input$dist_pop_rrt, input$n_rrt,
                                 input$k_rrt, input$tingkat_keper_rrt,
                                 input$sigma_rrt)
    persen_mencakup <- mean(sapply(data_sk,
                                   function(sk)
                                     sk$mencakup_rrt_populasi)) * 100
    paste("Gambar di atas memvisualisasikan ", k, " selang kepercayaan ", tingkat_keper, "% dari tiap-tiap sampel yang terpilih. Setiap sampel tersebut dipilih secara acak dari sebuah populasi yang berdistribusi ", dist_pop, " dengan rerata ", round(rrt_pop, 2), " dan simpangan baku ", round(sd_pop, 2), ". Persentase selang kepercayaan yang mencakup rerata populasi: ", round(persen_mencakup, 2), "%", sep = "")
  })
}

# Membuat objek aplikasi Shiny ----
shinyApp(ui = ui, server = server)