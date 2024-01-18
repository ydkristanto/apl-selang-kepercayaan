# Paket yang diperlukan ----
library(shiny)
library(ggplot2)

# Mendefinisikan UI ----
ui <- fluidPage(
  titlePanel("Selang Kepercayaan Proporsi"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        sliderInput("p_prop", "Proporsi populasi:",
                    0.5, min = 0, max = 1, step = 0.01)
      ),
      wellPanel(
        selectInput("tingkat_keper_prop", "Tingkat kepercayaan:",
                    choices = c("90%" = "0.90",
                                "95%" = "0.95",
                                "99%" = "0.99"),
                    selected = "0.95")
      ),
      wellPanel(
        sliderInput("n_prop", "Ukuran sampel:",
                    30, min = 10, max = 100, step = 5),
        sliderInput("k_prop", "Banyak sampel:",
                    20, min = 10, max = 500, step = 10)
      )
    ),
    
    mainPanel(
      plotOutput("plot_cakupan_prop"),
      textOutput("teks_cakupan_prop")
    )
  )
)

# Mendefinisikan fungsi peladen ----
seed = as.numeric(Sys.time())
server <- function(input, output) {
  # Fungsi untuk membuat data dan menentukan selang kepercayaan
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
}

shinyApp(ui, server)
