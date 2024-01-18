# Paket yang diperlukan ----
library(shiny)
library(ggplot2)

# Mendefinisikan UI ----
ui <- fluidPage(
  titlePanel("Selang Kepercayaan Proporsi"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("p_prop", "Proporsi populasi:",
                  0.5, min = 0, max = 1, step = 0.01),
      sliderInput("n_prop", "Ukuran sampel",
                  30, min = 10, max = 100, step = 5),
      sliderInput("k_prop", "Banyak sampel:",
                  20, min = 10, max = 500, step = 10),
      selectInput("tingkat_keper_prop", "Tingkat kepercayaan:",
                  choices = c("90%" = "0.90",
                              "95%" = "0.95",
                              "99%" = "0.99"))
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
  generate_data <- function(p, n_prop, k_prop, tk_prop) {
    set.seed(seed)
    samples <- matrix(rbinom(n_prop * k_prop, size = 1, prob = p),
                      ncol = k_prop)
    ci_data <- lapply(1:k_prop, function(i) {
      sample <- samples[, i]
      prop_test <- prop.test(sum(sample), length(sample),
                             conf.level = as.numeric(tk_prop))
      ci <- prop_test$conf.int
      mean_value <- mean(sample)
      is_covering <- p >= ci[1] && p <= ci[2]
      data.frame(x = i, xend = i, y = ci[1], yend = ci[2], is_covering = is_covering, mean_value = mean_value)
    })
    
    return(ci_data)
  }
  
  # Plot cakupan selang kepercayaan
  output$plot_cakupan_prop <- renderPlot({
    ci_data <- generate_data(input$p_prop, input$n_prop,
                             input$k_prop, input$tingkat_keper_prop)
    
    ggplot() +
      geom_segment(data = do.call(rbind, ci_data),
                   aes(x = x, xend = xend, y = y, yend = yend,
                       color = factor(is_covering)),
                   size = 1,
                   alpha = .6) +
      geom_point(data = do.call(rbind, ci_data),
                 aes(x = x, y = mean_value,
                     color = factor(is_covering)),
                 size = 2) +
      geom_hline(yintercept = input$p_prop, linetype = "dashed",
                 color = "black", linewidth = 1) +
      scale_color_manual(values = c("FALSE" = "#d95f02", "TRUE" = "#1b9e77")) +
      labs(title = "Cakupan selang kepercayaan proporsi",
           y = "Proporsi populasi") +
      theme_bw(base_size = 16) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "bottom")
  })
  
  # Display coverage percentage
  output$teks_cakupan_prop <- renderText({
    ci_data <- generate_data(input$p_prop, input$n_prop, input$k_prop, input$tingkat_keper_prop)
    covering_percentage <- mean(sapply(ci_data, function(ci) ci$is_covering)) * 100
    paste("Persentase selang kepercayaan yang mencakup proporsi populasi: ", round(covering_percentage, 2), "%", sep = "")
  })
}

shinyApp(ui, server)
