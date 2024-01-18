# app.R
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Visualisasi Selang Kepercayaan Rerata"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("ukuran_sampel", "Ukuran Sampel", value = 30, min = 10, max = 100, step = 1),
      sliderInput("n_sampel", "Jumlah Sampel", value = 100, min = 1, max = 500, step = 1),
      selectInput("tingkat_kepercayaan", "Tingkat Kepercayaan",
                  choices = c("90%" = "0.9",
                              "95%" = "0.95",
                              "99%" = "0.99")),
      radioButtons("sd_diketahui", "SD Populasi Diketahui",
                   choices = c("Ya", "Tidak"), inline = TRUE)
    ),
    
    mainPanel(
      plotOutput("plot_selang_kepercayaan")
    )
  )
)

server <- function(input, output) {
  ## Fungsi untuk rerata ----
  # Tetapkan parameter populasi
  mean_populasi <- 500
  sd_populasi <- 100
  
  # Fungsi untuk menghasilkan data dan menghitung selang kepercayaan
  hasilkan_data <- function(ukuran_sampel, n_sampel, tingkat_kepercayaan, sd_diketahui) {
    set.seed(123)  # Atur seed untuk reproduktibilitas
    
    # Hasilkan sampel dan hitung selang kepercayaan
    data_selang_kepercayaan <- lapply(1:n_sampel, function(i) {
      sampel <- rnorm(ukuran_sampel, mean = mean_populasi, sd = sd_populasi)
      rerata_val <- mean(sampel)
      
      if (sd_diketahui == "Ya") {
        selang_kepercayaan <- qnorm(c(0.5 - as.numeric(tingkat_kepercayaan)/2, 0.5 + as.numeric(tingkat_kepercayaan)/2),
                                    rerata_val, sd = sd(sampel)/sqrt(ukuran_sampel))
      } else {
        selang_kepercayaan <- t.test(sampel, conf.level = as.numeric(tingkat_kepercayaan))$conf.int
      }
      
      # Periksa apakah selang kepercayaan mencakup rerata populasi
      mencakup_populasi <- selang_kepercayaan[1] <= mean_populasi && selang_kepercayaan[2] >= mean_populasi
      
      data.frame(x = i, xend = i, y = selang_kepercayaan[1], yend = selang_kepercayaan[2], rerata_val = rerata_val, mencakup_populasi = mencakup_populasi)
    })
    
    return(data_selang_kepercayaan)
  }
  
  # Plot selang kepercayaan
  output$plot_selang_kepercayaan <- renderPlot({
    data_selang_kepercayaan <- hasilkan_data(input$ukuran_sampel, input$n_sampel,
                                             input$tingkat_kepercayaan, input$sd_diketahui)
    
    ggplot() +
      geom_segment(data = do.call(rbind, data_selang_kepercayaan),
                   aes(x = x, xend = xend, y = y, yend = yend, color = ifelse(mencakup_populasi, "green", "red")),
                   size = 1) +
      geom_point(data = do.call(rbind, data_selang_kepercayaan),
                 aes(x = x, y = rerata_val, color = ifelse(mencakup_populasi, "green", "red")),
                 size = 3) +
      geom_hline(yintercept = mean_populasi, linetype = "dashed", color = "green") +
      labs(title = "Visualisasi Selang Kepercayaan Rerata",
           x = "Nomor Sampel", y = "Nilai") +
      scale_color_identity() +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "black"),
            text = element_text(color = "black"))
  })
}

shinyApp(ui, server)
