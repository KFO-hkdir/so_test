
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

dt <- read.csv("data/dt.csv", stringsAsFactors = FALSE)

############################################
#### ALTERNATIVT OPPSETT - MED SHINYLIVE ###
############################################

make_summary <- function(dt_filtered) {
  sokere_aar <- dt_filtered %>%
    filter(status == "AKT") %>%
    group_by(aar) %>%
    summarise(n_sokere = n_distinct(regnr), .groups = "drop")
  
  studplass_aar <- dt_filtered %>%
    filter(status == "AKT") %>%
    group_by(aar) %>%
    distinct(studiekode, studieplasser) %>%
    summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE), .groups = "drop")
  
  kvalifisert_aar <- dt_filtered %>%
    filter(status == "AKT") %>%
    group_by(aar, regnr) %>%
    summarise(is_qualified = any(kvalifisert == "J"), .groups = "drop_last") %>%
    summarise(kvalifiserte_søkere = sum(is_qualified), .groups = "drop")
  
  tilbud_aar <- dt_filtered %>%
    filter(tilbud == "J") %>%
    group_by(aar) %>%
    summarise(tilbud = n_distinct(regnr), .groups = "drop")
  
  tilbud_forste_aar <- dt_filtered %>%
    filter(prioritet == 1 & tilbud == "J") %>%
    group_by(aar) %>%
    summarise(tilbud_førstevalg = n_distinct(regnr), .groups = "drop")
  
  dt_all <- sokere_aar %>%
    left_join(kvalifisert_aar, by = "aar") %>%
    left_join(studplass_aar, by = "aar") %>%
    left_join(tilbud_aar, by = "aar") %>%
    left_join(tilbud_forste_aar, by = "aar") %>%
    mutate(across(-aar, ~replace_na(.x, 0)))
  
  dt_all %>%
    pivot_longer(cols = -aar, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(
      variable,
      levels = c("n_sokere", "kvalifiserte_søkere",
                 "antall_studieplasser", "tilbud", "tilbud_førstevalg"),
      labels = c("Søkere", "Kvalifiserte søkere", "Studieplasser",
                 "Søkere med tilbud", "Søkere med tilbud (førstevalg)")
    ))
}

ui <- fluidPage(
  titlePanel("Søkere / tilbud per år — Shinylive-versjon"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_utd", "Fagområde", choices = NULL),
      checkboxInput("show_all_vars", "Vis alle variabler", TRUE)
    ),
    mainPanel(
      plotOutput("line_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  updateSelectInput(session, "selected_utd",
                    choices = sort(unique(dt$utd_omr)),
                    selected = sort(unique(dt$utd_omr))[1])
  
  dt_filtered <- reactive({
    req(input$selected_utd)
    dt %>% filter(utd_omr == input$selected_utd)
  })
  
  dt_rs_filtered <- reactive({
    make_summary(dt_filtered())
  })
  
  output$line_plot <- renderPlot({
    dt_rs <- dt_rs_filtered()
    req(nrow(dt_rs) > 0)
    
    max_val <- max(dt_rs$value, na.rm = TRUE)
    if (max_val <= 0) max_val <- 1
    
    if (!input$show_all_vars) {
      dt_rs <- dt_rs %>% filter(variable == "Søkere")
    }
    
    ggplot(dt_rs, aes(x = aar, y = value, color = variable)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      labs(x = NULL, y = NULL,
           title = paste("Fagområde:", input$selected_utd)) +
      scale_x_continuous(breaks = unique(dt_rs$aar)) +
      scale_y_continuous(limits = c(0, max_val * 1.1)) +
      theme_minimal(base_size = 15)
  })
}

shinyApp(ui, server)