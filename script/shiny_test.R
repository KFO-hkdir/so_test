

Sys.setlocale("LC_ALL", "nb_NO.UTF-8")
getwd()
library(shiny)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(tidyverse)
library(readxl)
library(purrr)
library(pastecs)
library(Hmisc)
library(psych)
library(magrittr)
library(dplyr)
library(gt)
library(shinythemes)
library(knitr)
library(ranger)
library(rmarkdown)
library(bslib)
library(DataExplorer)
library(openxlsx)
library(fst)
library(htmlwidgets)
library(plotly)
library(scales)
library(shiny)
library(shinylive)
library(shinythemes)

dt <- read_fst("data/so_uhg_hoved_master.fst")


# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)


####################
### LINJEDIAGRAM ###
####################

### Søkere/førstevalgssøkere og planlagte studieplasser - nasjonalt ###

## Søkere per år

sokere_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar) %>%
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()

# Antall studier (minus trukne studier)
studier_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar) %>%
  summarise(antall_studier = n_distinct(studiekode)) %>% 
  print()

# Antall trukne studier 
studier_aar <- dt %>%
  filter(status=="TRU") %>% 
  group_by(aar) %>%
  summarise(antall_studier = n_distinct(studiekode)) %>% 
  print()

# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()

# Antall kvalifiserte førstevalgssøkere
kvalifisert_forste_aar <- dt %>% 
  filter(prioritet==1 & kvalifisert=="J") %>% 
  group_by(aar) %>% 
  summarise(kvalifiserte_førstesøkere = n_distinct(regnr)) %>% 
  print()


# Antall søkere med tilbud (på ett alternativ)
tilbud_aar <- dt %>% 
  filter(tilbud=="J") %>% 
  group_by(aar) %>% 
  summarise(tilbud = n_distinct(regnr)) %>% 
  print()


# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(prioritet==1 & tilbud=="J") %>% 
  group_by(aar) %>% 
  summarise(tilbud_førstevalg = n_distinct(regnr)) %>% 
  print()

# Antall kvalifiserte søkere uten tilbud
kvalifisert_ikketilbud_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, regnr) %>%
  filter(all(tilbud=="N")) %>% 
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalif_søkere_ikktilbud = sum(is_qualified)) %>% 
  print()

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(kvalifisert_aar, by = "aar") %>% 
  left_join(studplass_aar, by = "aar") %>%
  left_join(tilbud_aar, by = "aar") %>% 
  left_join(tilbud_forste_aar, by = "aar") 
dt_all

# 🔄 Reshape to long format for plotting
dt_rs <- dt_all %>%
  pivot_longer(cols = -aar,
               names_to = "variable",
               values_to = "value")

dt_rs %>% 
  print(n=30)

# Endrer på rekkefølge + navn til labels 
dt_rs <- dt_rs %>%
  mutate(variable = factor(variable,
                           levels = c("n_sokere", 
                                      "kvalifiserte_søkere",
                                      "antall_studieplasser", 
                                      "tilbud",
                                      "tilbud_førstevalg"),
                           labels = c("Søkere", 
                                      "Kvalifiserte søkere",
                                      "Studieplasser", 
                                      "Søkere med tilbud",
                                      "Søkere med tilbud (førstevalg)")))

dt_rs %>% 
  print(n=30)

## Forenklet ggplot 
fig_nasj_uhg_alt <- ggplot(dt_rs, aes(x = aar, y = value, color = variable)) +
  geom_line(size = 2) +
  geom_point(
    size = 5,
    aes(text = paste0(variable, ": ", scales::comma(value)))   # tooltip here
  ) +
  scale_color_manual(values = hkdir_farger[1:6]) +
  labs(x = "", y = "", color = "") +
  scale_y_continuous(limits = c(0, 170000)) +
  scale_x_continuous(
    breaks = unique(dt_rs$aar),
    labels = as.integer(unique(dt_rs$aar))
  ) +
  theme_minimal(base_size = 14)

fig_nasj_uhg_alt_widget <- plotly::ggplotly(fig_nasj_uhg_alt, tooltip = "text")
fig_nasj_uhg_alt_widget


fig_nasj_uhg_alt_widget <- fig_nasj_uhg_alt_widget %>%
  layout(
    legend = list(
      orientation = "h",       # horizontal
      x = 0.5,                 # center horizontally
      xanchor = "center",
      y = -0.2                 # push it below the plot
    )
  )

# fig_nasj_uhg_alt_widget %>% 
#   layout(hovermode = "closest")
# saveWidget(fig_nasj_uhg_alt_widget, "figurer/fig_nasj_uhg_alt.html", selfcontained = TRUE)


#####################################################
### LOOP MED FIGURER FORDELT PÅ UTD_OMR (FUNKSJON ###
#####################################################

make_summary <- function(dt_filtered) {
  # Ensure expected columns exist in dt
  # This replicates your summarise pipeline and returns a long-format dataframe ready for plotting
  
  
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
  
  
  # merge
  dt_all <- sokere_aar %>%
    left_join(kvalifisert_aar, by = "aar") %>%
    left_join(studplass_aar, by = "aar") %>%
    left_join(tilbud_aar, by = "aar") %>%
    left_join(tilbud_forste_aar, by = "aar")
  
  
  # Replace NA with 0 for numeric columns to avoid plotting problems
  dt_all <- dt_all %>%
    mutate(across(-aar, ~ replace_na(.x, 0)))
  
  
  # pivot longer
  dt_rs <- dt_all %>%
    pivot_longer(cols = -aar, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable,
                             levels = c("n_sokere",
                                        "kvalifiserte_søkere",
                                        "antall_studieplasser",
                                        "tilbud",
                                        "tilbud_førstevalg"),
                             labels = c("Søkere",
                                        "Kvalifiserte søkere",
                                        "Studieplasser",
                                        "Søkere med tilbud",
                                        "Søkere med tilbud (førstevalg)")))
  
  
  return(dt_rs)
}

#######################
### LAGER SHINY-APP ###
#######################



# ----------------------
# UI
# ----------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("/* tiny style tweaks for embedding */
body { background: #ffffff; }
.container { max-width: 1100px; margin: 10px auto; }
.plotly { height: 600px !important; }"))
  ),
  div(class = "container",
      titlePanel("Søkere / tilbud per år — velg fagområde"),
      
      
      sidebarLayout(
        sidebarPanel(
          uiOutput("utd_omr_ui"),
          checkboxInput("show_all_vars", "Vis alle variabler (alle linjer)", value = TRUE),
          br(),
          helpText("Velg fagområde (utd_omr). Appen genererer samme type linjediagrammer
som i ditt skript, men filtrert per fagområde.")
        ),
        
        
        mainPanel(
          plotlyOutput("line_plot", height = "650px"),
          br(),
          verbatimTextOutput("debug")
        )
      )
  )
)

#----------------------
# Server
# ----------------------

server <- function(input, output, session) {
  # Expect that `dt` is available in global environment. If not, show an informative error.
  if (!exists("dt")) {
    stop("Data object `dt` not found in the app environment. Load your dataset into `dt` before launching the app.")
  }
  
  
  # If utd_omr doesn't exist, create a placeholder
  if (!"utd_omr" %in% names(dt)) {
    dt$utd_omr <- "Alle"
  }
  
  
  utd_choices <- reactive({
    # get unique values and sort
    sort(unique(dt$utd_omr))
  })
  
  
  output$utd_omr_ui <- renderUI({
    selectInput("selected_utd",
                "Fagområde (utd_omr)",
                choices = c("-- Velg --" = "", utd_choices()),
                selected = utd_choices()[1])
  })
  
  
  # reactive filtered dt
  dt_filtered <- reactive({
    req(input$selected_utd)
    if (input$selected_utd == "") {
      # if nothing selected, return whole dataset
      dt
    } else {
      dt %>% filter(utd_omr == input$selected_utd)
    }
  })
  dt_rs_filtered <- reactive({
    # create the summarized long data for the filtered dataset
    make_summary(dt_filtered())
  })
  
  
  output$line_plot <- renderPlotly({
    dt_rs <- dt_rs_filtered()
    
    
    # prevent errors when no rows
    validate(need(nrow(dt_rs) > 0, "Ingen data for valgt fagområde."))
    
    
    # dynamic y limit: base on max value in this filtered dataset
    max_val <- max(dt_rs$value, na.rm = TRUE)
    # if max_val is 0 (all zeros), set a small upper limit so plot shows nicely
    if (is.na(max_val) || max_val <= 0) max_val <- 1
    
    
    # fallback color palette if hkdir_farger not present
    if (exists("hkdir_farger")) {
      pal <- hkdir_farger
    } else {
      pal <- RColorBrewer::brewer.pal(5, "Set1")
    }
    p <- ggplot(dt_rs, aes(x = aar, y = value, color = variable, group = variable)) +
      geom_line(size = 1.8) +
      geom_point(size = 3,
                 aes(text = paste0(variable, ": ", scales::comma(value)))) +
      scale_color_manual(values = pal) +
      labs(x = NULL, y = NULL, color = NULL,
           title = paste0("Fagområde: ", ifelse(input$selected_utd=="", "(hele dataset)", input$selected_utd))) +
      scale_x_continuous(breaks = unique(dt_rs$aar), labels = as.integer(unique(dt_rs$aar))) +
      scale_y_continuous(limits = c(0, max_val * 1.05)) +
      theme_minimal(base_size = 13)
    
    
    if (!input$show_all_vars) {
      # allow user to only show "Søkere" e.g. - here we default to showing only Søkere
      p <- p + scale_color_manual(values = pal) +
        geom_line(data = dt_rs %>% filter(variable == "Søkere"), aes(x = aar, y = value, color = variable), size = 1.8) +
        geom_point(data = dt_rs %>% filter(variable == "Søkere"), aes(x = aar, y = value, text = paste0(variable, ": ", scales::comma(value))), size = 3)
    }
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
             hovermode = "closest")
  })
  output$debug <- renderPrint({
    # small helpful debug info
    cat("Valgt fagområde:", input$selected_utd, "\n")
    cat("Rader i filtrert dataset:", nrow(dt_filtered()), "\n")
    if (exists("dt_rs_filtered")) {
      cat("Maks verdi i plott:", max(dt_rs_filtered()$value, na.rm = TRUE), "\n")
    }
  })
}


# Run the app
fig_app <- shinyApp(ui = ui, server = server)
fig_app





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
