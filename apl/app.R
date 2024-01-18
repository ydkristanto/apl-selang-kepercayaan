# Memanggil paket ----
library(shiny)
library(tidyverse)
library(gridExtra)

# Mendefinisikan UI ----
ui <- shinyUI(fluidPage(
  title = "Demonstrasi Teorema Limit Pusat -- Aplikasi Shiny",
  navbarPage("Cakupan Selang Kepercayaan",
             position = "static-top",
             ## Tab proporsi ----
             tabPanel("Proporsi",
                      sidebarPanel(
                        wellPanel(
                          sliderInput("p_prop", "Proporsi populasi:",
                                      value = .5,
                                      step = .01,
                                      min = 0,
                                      max = 1)
                        ),
                        wellPanel(
                          sliderInput("n_prop", "Ukuran sampel:",
                                      value = 30,
                                      step = 1,
                                      min = 2,
                                      max = 100),
                          hr(),
                          sliderInput("k_prop", "Banyak sampel:",
                                      value = 20,
                                      step = 1,
                                      min = 10,
                                      max = 100)
                        ),
                        wellPanel(
                          radioButtons("level", "Tingkat kepercayaan:",
                                       choices = c("90%" = ".9",
                                                   "95%" = ".95",
                                                   "99%" = ".99"))
                        )
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Cakupan",
                                             br(),
                                             plotOutput("conf.int",
                                                        height = "540px")),
                                    tabPanel("Distribusi Populasi",
                                             br(),
                                             plotOutput("pop.dist.prop",
                                                        height = "450px")),
                                    tabPanel("Beberapa Sampel",
                                             br(),
                                             plotOutput("sample.dist.prop"),
                                             br(),
                                             div(h4(textOutput("num.samples.prop")),
                                                 align ="center")),
                                    tabPanel("Distribusi Sampling",
                                             br(),
                                             plotOutput("sampling.dist.prop"),
                                             div(textOutput("plot.descr.prop"),
                                                 align = "center"),
                                    )
                        )
                      )
             ),
             ## Tab rerata ----
             tabPanel("Rerata",
                      sidebarLayout(
                        sidebarPanel(
                          wellPanel(
                            ### Memilih distribusi ----
                            radioButtons("dist", "Distribusi populasi:",
                                         c("Normal" = "rnorm",
                                           "Seragam" = "runif",
                                           "Condong ke kanan" = "rlnorm",
                                           "Condong ke kiri" = "rbeta",
                                           "Puncak ganda" = "rnorm2"),
                                         selected = "rnorm"),
                            hr(),
                            ### Parameter distribusi ----
                            uiOutput("mu"),
                            uiOutput("sd"),
                            uiOutput("minmax"),
                            uiOutput("skew"),
                            uiOutput("shape")
                          ),
                          wellPanel(
                            ### Memilih ukuran sampel ----
                            sliderInput("n_rrt",
                                        "Ukuran sampel:", 
                                        value = 30,
                                        min = 2,
                                        max = 500),
                            hr(),
                            ### Menentukan banyak sampel ----
                            sliderInput("k_rrt",
                                        "Banyaknya sampel:",
                                        value = 200,
                                        min = 10,
                                        max = 1000)
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            type = "tabs",
                            ### Tab distribusi populasi ----
                            tabPanel(title = "Distribusi Populasi",
                                     br(),
                                     plotOutput("pop.dist.rrt",
                                                height = "500px"),
                                     br()
                            ),
                            ### Tab beberapa sampel ----
                            tabPanel(title = "Beberapa Sampel",
                                     br(),
                                     plotOutput("sample.dist.rrt"),
                                     br(),
                                     div(h4(textOutput("num.samples.rrt")),
                                         align = "center"),
                            ),
                            ### Tab distribusi sampling ----
                            tabPanel(title = "Distribusi Sampling",
                                     fluidRow(
                                       column(width = 12,
                                              br(),
                                              plotOutput("sampling.dist.rrt"),
                                              div(textOutput("sampling.descr.rrt",
                                                             inline = TRUE),
                                                  align = "center"))
                                     )
                            )
                          )
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
                          width = 6
                        )
                      )
             )
  )
)
)

# Fungsi peladen ----
seed = as.numeric(Sys.time())
server <- shinyServer(function(input, output) {
  ## Fungsi untuk proporsi ----
  rand_draw_prop = function(n, p)
  {
    vals = NULL
    vals = do.call(rbinom, list(n = n, size = 1, prob = p))      
    return(vals)
  }
  rep_rand_draw_prop = repeatable(rand_draw_prop)  
  parent_prop = reactive({
    n = 1e5
    return(rep_rand_draw_prop(input$n_prop, input$p_prop))
  })
  samples_prop = reactive({
    pop = parent_prop()
    n = input$n_prop
    k = input$k_prop
    return(replicate(k, sample(pop, n, replace = TRUE)))
  })
  ### Plot selang kepercayaan ----
  output$conf.int = renderPlot({
    k_prop = input$k_prop
    x = samples_prop()
    conf_int_lwr_prop = c()
    conf_int_upr_prop = c()
    for (i in k_prop) {
      conf_int_lwr_prop[i] = prop.test(sum(x[, i]), k_prop,
                                    correct = FALSE)$conf.int[1]
      conf_int_upr_prop[i] = prop.test(sum(x[, i]), k_prop,
                                       correct = FALSE)$conf.int[2]
    }
    conf_int_prop = tibble(sample = 1:k_prop,
                           lwr = conf_int_lwr_prop,
                           upr = conf_int_upr_prop)
    ggplot(conf_int_prop) +
      geom_segment(aes(x = sample, y = lwr,
                       xend = sample, yend = upr))
  })
  ### Plot populasi proporsi ----
  output$pop.dist.prop = renderPlot({
    popsize = 1000
    counts = data.frame(number = c("0","1"),
                        freq = c(popsize * (1 - input$p_prop),
                                 popsize * input$p_prop) / popsize)
    ggplot(counts, aes(x = number, y = freq, fill = factor(number))) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Frekuensi Relatif",
           title = paste0("Distribusi populasi: p = ", input$p_prop),
           size = 14, face = "bold") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_brewer(palette = "Dark2") +
      theme_bw(base_size = 16) +
      theme(legend.position = "none")
  })
  ### Plot beberapa sampel proporsi ----
  output$sample.dist.prop = renderPlot({
    x = samples_prop()
    plot <- list()
    for(i in 1:8){
      df <- tibble(obs = x[,i])
      counts <- df %>% count(obs)
      plot[[i]] <- ggplot(counts, aes(x = obs, y = n, fill = factor(obs))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, 1.2 * max(counts$n))) +
        scale_x_discrete(limits = c(0, 1)) +
        scale_fill_brewer(palette = "Dark2") +
        theme_bw(base_size = 12) +
        theme(legend.position = "none") +  
        labs(x = "",  y = "Frekuensi",
             title = paste("Sampel",i), size = 14, face = "bold")
      mean_samp = round(mean(x[,i]),2)
      sd_samp = round(sd(x[,i]),2)
      y_pos = max(counts$n) + 0.07 * max(counts$n)
      # #added if statement to check if count 1 or count 2 are NA. this check
      # #eliminated the error messages in the app
      if(!is.na(counts$n[1]) & !is.na(counts$n[2])) {
        if(counts$n[1] > counts$n[2]) {
          plot[[i]] <- plot[[i]] +
            annotate("text", x = 1, y = y_pos,
                     label = paste("p_topi =",
                                   bquote(.(mean_samp))),
                     color ="black", size = 3) 
        }
        else {
          plot[[i]] <- plot[[i]] +
            annotate("text", x = 0, y = y_pos,
                     label = paste("p_topi =" ,
                                   bquote(.(mean_samp))),
                     color = "black", size = 3) 
        }}
      else {
        plot[[i]] <- plot[[i]] +
          annotate("text", x = 0.5, y = y_pos,
                   label = paste("p_topi =" , bquote(.(mean_samp))),
                   color = "black", size = 3)
      }
    }
    grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]],
                 plot[[5]], plot[[6]], plot[[7]], plot[[8]],
                 ncol = 4)
  })
  # teks
  output$num.samples.prop = renderText({
    k = input$k_prop
    paste0("... dan seterusnya sampai sampel ke-", k,".")
  })
  ### Plot distribusi sampling proporsi ----
  output$sampling.dist.prop = renderPlot({
    n = input$n_prop
    p = input$p_prop
    k = input$k_prop
    pop = parent()
    ndist = tibble(means = colMeans(samples()))
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot = FALSE)
    
    m_samp = round(mean(ndist$means), 2)
    sd_samp = round(sd(ndist$means), 2)
    sd_teor = sqrt(p * (1 - p) / n)
    
    x_range = max(ndist$means) - min(ndist$means)
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = if_else(m_samp > 0, min(ndist$means) + 0.1*x_range, max(ndist$means) - 0.1*x_range)
    
    # minor change in the way the title is displayed
    
    ggplot(ndist, aes(x = ndist$means)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 20, color ="white") +
      stat_density(geom = "line", size = 1) +
      labs(title = paste("Distribusi sampling proporsi*"),
           x = "Proporsi sampel",
           y = "") +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("rerata p_topi","=", bquote(.(m_samp)),"\n", "SD p_topi ", "=", bquote(.(sd_samp))),
               color = "black", size = 5) +
      theme_bw(base_size = 17) 
  })
  
  # text
  output$plot.descr.prop = renderText({
    n = input$n_prop
    p = input$p_prop
    k = input$k_prop
    
    paste("*Distribusi proporsi ", k, 
          " sampel acak, masing-masing\nmemuat ", n, 
          " observasi dari populasi", sep = "")
    
  })
  ## Fungsi untuk rerata ----
  ### Slider rerata untuk distribusi normal ----
  output$mu = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("mu",
                    "Rerata:",
                    value = 0,
                    min = -40,
                    max = 50)
      }
    })
  
  ### Slider simpangan baku untuk distribusi normal ----
  output$sd = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("sd",
                    "Simpangan baku:",
                    value = 20,
                    min = 1,
                    max = 30)
      }
    })
  
  ### Slider minmaks untuk distribusi seragam ----
  output$minmax = renderUI(
    {
      
      if (input$dist == "runif")
      {
        sliderInput("minmax",
                    "Batas bawah dan batas atas",
                    value = c(5, 15),
                    min = 0,
                    max = 20)
      }
    })
  
  ### Memastikan jangkauan untuk distribusi seragam != 0 ----
  observeEvent(input$minmax, {
    
    req(input$minmax)
    
    if (input$minmax[1] == input$minmax[2]){
      if (input$minmax[1] == 0){
        updateSliderInput(session, "minmax", value = c(0, 1))
      } else if (input$minmax[2] == 20){
        updateSliderInput(session, "minmax", value = c(19, 20))
      } else {
        updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
      }
    }
  })
  
  ### Slider kecondongan untuk rlnorm dan rbeta ----
  output$skew = renderUI(
    {
      
      if (input$dist == "rlnorm" | input$dist == "rbeta"){
        selectInput(inputId = "skew",
                    label = "Kecondongan:",
                    choices = c("Rendah" = "low",
                                "Sedang" = "med",
                                "Tinggi" = "high"),
                    selected = "low")
      }
    })
  
  ### Pilihan bentuk untuk rnorm2 ----
  output$shape = renderUI(
    {
      
      if (input$dist == "rnorm2"){
        selectInput(inputId = "shape",
                    label = "Bentuk:",
                    choices = c("Bentuk 1" = "shape1",
                                "Bentuk 2" = "shape2",
                                "Bentuk 3" = "shape3"),
                    selected = "shape1")
      }
    })
  
  ### Membuat sampel-sampel random ----
  # Mendefinisikan fungsi untuk distribusi puncak ganda
  rnorm2 <- function(n, mu1, mu2, sd){
    return(c(rnorm(n = round(2/5*n), mean = mu1, sd = sd),
      rnorm(n = n - round(2/5*n), mean = mu2, sd = sd)))
  }
  rand_draw_rrt <- function(dist, n, mu, sd, min, max, skew, shape){
    
    vals = NULL
    
    if (dist == "rbeta"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n = n, shape1 = 5, shape2 = 2))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n = n, shape1 = 5, shape2 = 1.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n = n, shape1 = 5, shape2 = 1)) 
      }
    }
    
    else if (dist == "rnorm"){
      req(mu, sd)
      vals = do.call(dist, list(n = n, mean = mu, sd = sd))
    }
    
    else if (dist == "rlnorm"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n = n, meanlog = 0, sdlog = .25))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n = n, meanlog = 0, sdlog =.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n = n, meanlog = 0, sdlog = 1))
      }
    }
    
    else if (dist == "runif"){
      req(min, max)
      vals = do.call(dist, list(n = n, min = min, max = max))
    }
    else if (dist == "rnorm2"){
      req(shape)
      if (shape == "shape1"){
        vals = do.call(dist, list(n = n, mu1 = 30, mu2 = 70, sd = 8))
      }
      else if (shape == "shape2"){
        vals = do.call(dist, list(n = n, mu1 = 70, mu2 = 30, sd = 10))
      }
      else if (shape == "shape3"){
        vals = do.call(dist, list(n = n, mu1 = 30, mu2 = 70, sd = 12))
      }
    }
    return(vals)
  }
  
  rep_rand_draw_rrt = repeatable(rand_draw_rrt)
  
  ### Mendefinisikan beberapa variabel reaktif lainnya ----
  parent_rrt = reactive({
    n_sample_rrt = 1e5
    return(rep_rand_draw_rrt(input$dist, n_sample_rrt, input$mu, input$sd,
                             input$minmax[1], input$minmax[2], input$skew,
                             input$shape))
  })
  samples_rrt = reactive({
    pop = parent_rrt()
    n = input$n_rrt
    k = input$k_rrt
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  u_min = reactive({
    req(input$minmax)
    return(input$minmax[1])
  })
  u_max = reactive({
    req(input$minmax)
    return(input$minmax[2])
  })
  ### plot 1 a) ----
  output$pop.dist.rrt = renderPlot({
    distname = switch(input$dist,
                      rnorm = "Distribusi populasi: Normal",
                      rlnorm = "Distribusi populasi: Condong ke kanan",
                      rbeta = "Distribusi populasi: Condong ke kiri",
                      runif = "Distribusi populasi: Seragam",
                      rnorm2 = "Distribusi populasi: Puncak ganda")
    pop = parent_rrt()
    
    m_pop =  round(mean(pop), 2)
    sd_pop = round(sd(pop), 2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                     max(100, max(pop$samples)) - 20)
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white") +
        # geom_density() + draws a weird baseline. using stat_density() instead.
        stat_density(geom = "line", size = 2) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("rerata x", "=", bquote(.(m_pop)),
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_bw(base_size = 19)
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        "  " # this is to temporarily prevent graph from displaying while 
        # observeEvent is fixing the range.
      } else {
        
        x_pos = max(pop$samples) - 0.1*x_range
        
        ggplot(data = pop, aes(x = samples, y = ..density..)) +
          geom_histogram(bins = 45, color = "white") +
          stat_density(geom = "line", size = 2) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("rerata x", "=", bquote(.(m_pop)),
                                 "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                   color = "black", size = 5) +
          theme_bw(base_size = 19)
      }
      
    } else if (input$dist == "rlnorm"){
      
      x_pos = max(pop$samples) - 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom = "line", size = 2) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("rerata x", "=", bquote(.(m_pop)), 
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_bw(base_size = 19)
      
    } else if (input$dist == "rbeta"){
      
      x_pos = min(pop$samples) + 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom = "line", size = 2) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("rerata x", "=", bquote(.(m_pop)), 
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_bw(base_size = 19)
      
    } else if (input$dist == "rnorm2"){
      
      x_pos = min(pop$samples) + 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom = "line", size = 2) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("rerata x", "=", bquote(.(m_pop)), 
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_bw(base_size = 19)
      
    }
  })
  ### Plot beberapa sampel ----
  output$sample.dist.rrt = renderPlot({
    
    y = samples_rrt()
    x = samples_rrt() %>% as_tibble()
    
    plots = list(rep(NA, 8))
    
    for(i in 1:8){
      
      mean = round(mean(y[,i]), 2)
      sd = round(sd(y[,i]), 2)
      
      x_range = max(y[,i]) - min(y[,i])
      pdens = density(y[,i])
      
      x_pos = ifelse(input$dist == "rbeta", min(y[,i]) + 0.1*x_range, 
                     max(y[,i]) - 0.1*x_range)
      
      plots[[i]] = ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.8, dotsize = 0.7,
                     fill = "#1b9e77", color = "#1b9e77") +
        geom_segment(aes(x = mean(y[,i]), y = 0,
                         xend = mean(y[,i]), yend = Inf),
                     color = "#d95f02", alpha = .6) +
        labs(title = paste("Sampel", i, sep = " "), x = "", y = "") +
        theme_bw(base_size = 13) +
        annotate("text", x = x_pos, y = 1.8,
                 label = paste("x_bar", "=", bquote(.(mean)),
                               "\n", "SD", "=", bquote(.(sd))),
                 color = "black", size = 3) +
        scale_y_continuous(limits = c(0,2), breaks = NULL) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                 plots[[6]], plots[[7]], plots[[8]], ncol = 4)
  })
  
  
  
  # Teks untuk plot beberapa sampel
  output$num.samples.rrt = renderText({
    k = input$k_rrt
    paste0("... dan seterusnya sampai sampel ke-",k,".")
  })
  
  ### Plot distribusi sampling rerata ----
  output$sampling.dist.rrt = renderPlot({
    distname = switch(input$dist,
                      rnorm = "populasi normal",
                      rlnorm  = "populasi condong ke kanan",
                      rbeta = "populasi condong ke kiri",
                      runif = "populasi seragam")
    
    n = input$n_rrt
    k = input$k_rrt
    
    pop = parent_rrt()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    ndist = tibble(means = colMeans(samples_rrt()))
    
    m_samp =  round(mean(ndist$means),2)
    sd_samp = round(sd(ndist$means),2)
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot = FALSE)
    
    x_range = max(ndist$means) - min(ndist$means)
    
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                   max(ndist$means) - 0.1*x_range)
    
    p = ggplot(data = ndist, aes(x = means, y = after_stat(density))) +
      geom_histogram(bins = 20, color = "white", fill = "#d95f02",
                     alpha = .6) +
      stat_density(geom = "line", size = 2) +
      labs(title = paste("Distribusi sampling rerata*"),
           x = "Rerata sampel",
           y = "") +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("rerata x_bar", "=", bquote(.(m_samp)),
                             "\n", "simpangan baku x_bar", "=", bquote(.(sd_samp))),
               color = "black", size = 5) +
      theme_bw(base_size = 19)
    
    if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
        p
      }
    } else {
      p
    }
  })
  
  ### Deskripsi plot distribusi sampling rerata ----
  output$sampling.descr.rrt = renderText({
    
    distname = switch(input$dist,
                      rnorm = "populasi normal",
                      rlnorm  = "populasi condong ke kanan",
                      rbeta = "populasi condong ke kiri",
                      runif = "populasi seragam")
    
    k = input$k_rrt
    n = input$n_rrt
    paste("*Distribusi rerata dari", k, "sampel acak,
          masing-masing memuat", n, " observasi
          dari sebuah", distname)
  })
})

# Membuat objek aplikasi Shiny ----
shinyApp(ui = ui, server = server)