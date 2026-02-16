###########################################################
### Figurer til Søkertall - søking på utdanningsområder ###
###########################################################

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
library(tidyr)
library(htmltools)
library(stringr)


dt <- read_fst("data/so_sokertall_master.fst")


# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)

#### Lager nye verdier for utdanningsområde og -type

dt <- dt %>%
  mutate(
    utd_omr = recode(utd_omr,
                     "INFOTEKN" = "Informasjonsteknologi",
                     "LANDBRUK" = "Land- og havbruk",
                     "IDRETT" = "Idrettsfag",
                     "HISTORIE" = "Historiefag",
                     "SPRÅK" = "Språkfag",
                     "ESTETISK" = "Estetiske fag",
                     "HELSEFAG" = "Helsefag",
                     "PEDFAG" = "Pedagogiske fag",
                     "REISELIV" = "Reiselivsfag",
                     "LÆRER" = "Lærerutdanninger",
                     "SAMFUNN" = "Samfunnsfag",
                     "REALFAG" = "Realfag",
                     "MEDIEFAG" = "Mediefag",
                     "TEKNO" = "Teknologiske fag",
                     "ØKADM" = "Økonomisk-administrative fag",
                     "JUS" = "Juridiske fag"
    )
  )


dt <- dt %>%
  mutate(
    utd_type = case_when(
      utd_type == "INFOTEKN" ~ "Informasjonsteknologi",
      utd_type == "IDRETT" ~ "Idrettsfag",
      utd_type == "HISTORIE" ~ "Historiefag",
      utd_type == "SPRÅK" ~ "Språkfag",
      utd_type == "ESTETISK" ~ "Estetiske fag",
      utd_type == "PEDFAG" ~ "Pedagogiske fag",
      utd_type == "REISELIV" ~ "Reiselivsfag",
      utd_type == "LÆRER" ~ "Lærerutdanninger",
      utd_type == "SAMFUNN" ~ "Samfunnsfag",
      utd_type == "REALFAG" ~ "Realfag",
      utd_type == "MEDIEFAG" ~ "Mediefag",
      utd_type == "TEKNO" ~ "Teknologiske fag",
      utd_type == "ØKADM" ~ "Økonomisk-administrative fag",
      utd_type == "JUS" ~ "Juridiske fag",
      utd_type == "LANDOGHAVBRUK" ~ "Land- og havbruk",
      
      utd_type == "LÆRER - BHGLÆRER" ~ "Lærerutdanninger - barnehage",
      utd_type == "LÆRER - GRL1-7" ~ "Lærerutdanninger - GLU1-7",
      utd_type == "LÆRER - GRL5-10" ~ "Lærerutdanninger - GLU 5-10",
      utd_type == "LÆRER - FAGLÆRER" & str_detect(programnavn, regex("yrke", ignore_case = TRUE)) ~ "Lærerutdanninger - yrkesfaglærer",
      utd_type == "LÆRER - FAGLÆRER" ~ "Lærerutdanninger - LUPE 1-13",
      utd_type == "LÆRER - LEKTOR 8-13" ~ "Lærerutdanninger - lektor",
      utd_type == "LÆRER - ANNET" ~ "Lærerutdanninger - annet",
      utd_type == "LÆRER - LUPE1-13" ~ "Lærerutdanninger - LUPE 1-13",
      
      utd_type == "HELSEFAG - VERNEPL" ~ "Helsefag - vernepleie",
      utd_type == "HELSEFAG - SOSIONOM" ~ "Helsefag - sosionom",
      utd_type == "HELSEFAG - SYKEPL" ~ "Helsefag - sykepleie",
      utd_type == "HELSEFAG - BARNEVER" ~ "Helsefag - barnevern",
      utd_type == "HELSEFAG - ERGO" ~ "Helsefag - ergoterapi",
      utd_type == "HELSEFAG - MEDISIN" ~ "Helsefag - medisin",
      utd_type == "HELSEFAG - FYSIO" ~ "Helsefag - fysioterapi",
      utd_type == "HELSEFAG - ERNÆRING" ~ "Helsefag - ernæring",
      utd_type == "HELSEFAG - ANNET" ~ "Helsefag - annet",
      utd_type == "HELSEFAG - RADIO" ~ "Helsefag - radiologi",
      utd_type == "HELSEFAG - BIOING" ~ "Helsefag - bioingeniør",
      utd_type == "HELSEFAG - ODONT" ~ "Helsefag - odontologi",
      utd_type == "HELSEFAG - VETERIN" ~ "Helsefag - veterinær",
      utd_type == "HELSEFAG - AUDIO" ~ "Helsefag - audio",
      utd_type == "HELSEFAG - RESEPTAR" ~ "Helsefag - reseptar",
      utd_type == "HELSEFAG - ORTOPEDI" ~ "Helsefag - ortopedi",
      utd_type == "HELSEFAG - FARMASI" ~ "Helsefag - farmasi",
      
      utd_type == "TEKNO - INGENIØR" ~ "Teknologiske fag - ingeniør",
      utd_type == "TEKNO - SIVING" ~ "Teknologiske fag - sivilingeniør",
      utd_type == "TEKNO - ARKITEKT" ~ "Teknologiske fag - arkitekt",
      utd_type == "TEKNO - ANNET" ~ "Teknologiske fag - annet",
      utd_type == "TEKNO - MARITIM" ~ "Teknologiske fag - maritime",
      
      TRUE ~ utd_type
    )
  )


# Sjekker at alle utd_omr og utd_type har blitt kodet om
unique(dt$utd_omr)
unique(dt$utd_type)


folder_path <- "figurer/side_1_utdanningsomr/tabbed_utd_omr_linje"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

####################
### LINJEDIAGRAM ###
####################


### Lager mappe for å lagre figuren

folder_path <- "figurer/side_1_utdanningsomr/utd_omr_linje"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

### Søkere/førstevalgssøkere og planlagte studieplasser - nasjonalt ###

## Søkere per år

sokere_aar <- dt %>%
  group_by(aar) %>%
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  group_by(aar) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()


# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(studplass_aar, by = "aar")
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
                                      "antall_studieplasser"),
                           labels = c("Søkere", 
                                      "Studieplasser")))

dt_rs %>% 
  print(n=30)

## Forenklet ggplot 
fig_nasj_uhg_alt <- ggplot(dt_rs, aes(x = aar, y = value, color = variable)) +
  geom_line(size = 0.5) +
  geom_point(
    size = 1.8,
    aes(text = paste0(variable, ": ", scales::comma(value)))   # tooltip here
  ) +
  scale_color_manual(values = hkdir_farger[1:6]) +
  labs(x = "", y = "", color = "") +
  scale_y_continuous(limits = c(0, 170000)) +
  scale_x_continuous(
    breaks = unique(dt_rs$aar),
    labels = as.integer(unique(dt_rs$aar))
  ) +
  theme_minimal(base_size = 11)

fig_nasj_uhg_alt
fig_nasj_uhg_alt_widget <- plotly::ggplotly(fig_nasj_uhg_alt, tooltip = "text", responsive ="true")
fig_nasj_uhg_alt_widget


fig_nasj_uhg_alt_widget <- fig_nasj_uhg_alt_widget %>%
  layout(
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.12,        # was -0.2 (too far down for short figure)
      font = list(size = 11)
    ),
    margin = list(l = 50, r = 20, t = 20, b = 60)
  ) %>%  
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )



fig_nasj_uhg_alt_widget$width  <- "100%"   # dynamisk bredde
fig_nasj_uhg_alt_widget$height <- 450      # fast høyde 

css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"

fig_nasj_uhg_alt_widget <- htmlwidgets::prependContent(
  fig_nasj_uhg_alt_widget,
  htmltools::tags$style(htmltools::HTML(css))
)


fig_nasj_uhg_alt_widget %>% 
  layout(hovermode = "closest")
saveWidget(fig_nasj_uhg_alt_widget, file.path(folder_path, "fig_nasj_uhg.html"), selfcontained = TRUE)




###############################
### LINJEDIAGRAM - TABULERT ###
###############################


########################### UTDANNINGSOMRÅDER ##################################


# ------------------------
# Helper functions
# ------------------------

make_safe_filename <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^0-9A-Za-z_-]", "_", x)
}

make_summary <- function(dt_filtered) {
  sokere_aar <- dt_filtered %>%
    group_by(aar) %>%
    filter(prioritet==1) %>% 
    summarise(n_sokere = n_distinct(regnr), .groups = "drop")
  
  studplass_aar <- dt_filtered %>%
    group_by(aar) %>%
    distinct(studiekode, studieplasser) %>%
    summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE), .groups = "drop")

  
  sokere_aar %>%
    left_join(studplass_aar, by = "aar") %>%
    mutate(across(-aar, ~replace_na(.x, 0))) %>%
    pivot_longer(-aar, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(
      variable,
      levels = c(
        "n_sokere",
        "antall_studieplasser"
      ),
      labels = c(
        "Søkere",
        "Studieplasser"
      )
    ))
}

# ------------------------
# Generate plots
# ------------------------

utd_values <- unique(dt$utd_omr)
plots <- list()

folder_path <- "figurer/side_1_utdanningsomr/tabbed_utd_omr_linje"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

for (utd in utd_values) {
  
  dt_rs <- make_summary(dt %>% filter(utd_omr == utd))
  max_val <- max(dt_rs$value, na.rm = TRUE)
  if (max_val <= 0) max_val <- 1
  
  p <- ggplot(dt_rs, aes(aar, value, color = variable, group = variable)) +
    geom_line(size = 0.5) +
    geom_point(aes(text = paste(variable, ":", scales::comma(value))), size = 1.8) +
    scale_color_manual(values = hkdir_farger) +
    scale_y_continuous(limits = c(0, max_val * 1.05)) +
    scale_x_continuous(breaks = unique(dt_rs$aar)) +
    labs(title = paste("Utdanningsområde:", utd), x = "", y = "") +
    theme_minimal(base_size = 11)
  
  plots[[utd]] <- ggplotly(p, tooltip = "text") %>%
    layout(
      height = 420,
      showlegend = FALSE   # ✅ disable Plotly legend
    ) %>%  
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
  
  plots[[utd]]$width  <- "100%"   # dynamisk bredde
  plots[[utd]]$height <- 320      # fast høyde 
  
  css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"

plots[[utd]] <- htmlwidgets::prependContent(
  plots[[utd]],
  htmltools::tags$style(htmltools::HTML(css))
)

saveWidget(
  plots[[utd]],
  file = file.path(folder_path, paste0(make_safe_filename(utd), ".html")),
  selfcontained = TRUE
)
}

# ------------------------
# Shared legend (HTML)
# ------------------------

legend_items <- data.frame(
  label = c(
    "Førstevalgssøkere",
    "Studieplasser"
  ),
  color = hkdir_farger[1:2]
)

shared_legend <- tags$div(
  style = "
    display:flex;
    flex-wrap:wrap;
    gap:20px;
    margin-bottom:15px;
    font-size:14px;
  ",
  lapply(seq_len(nrow(legend_items)), function(i) {
    tags$div(
      style = "display:flex; align-items:center; gap:6px;",
      tags$span(style = paste0(
        "width:12px;height:12px;display:inline-block;background:",
        legend_items$color[i]
      )),
      tags$span(legend_items$label[i])
    )
  })
)

# ------------------------
# Generate tabbed interface (iframes)
# ------------------------

tabs <- list()
tab_panels <- list()

for (i in seq_along(utd_values)) {
  
  utd <- utd_values[i]
  
  tabs[[i]] <- tags$li(
    class = ifelse(i == 1, "active", ""),
    tags$a(
      `data-toggle` = "tab",
      href = paste0("#tab", i),
      utd
    )
  )
  
  tab_panels[[i]] <- tags$div(
    class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
    id = paste0("tab", i),
    tags$iframe(
      src = paste0(make_safe_filename(utd), ".html"),
      style = "width:100%; height:420px; border:none;"
    )
  )
}

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "build_time", content = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), # For at Git skal registrere endring
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  tags$body(
    tags$div(
      class = "container",
      #tags$h2("Interaktive linjediagrammer per fagområde"),
      tags$ul(class = "nav nav-tabs", tabs),
      tags$div(class = "tab-content", tab_panels),
      shared_legend                 # ✅ shared legend here
    )
  )
)

save_html(page, file.path(folder_path, "tabbed_utd_omr.html"))



#################### UTDANNINGSTYPER - HELSE ###################################

# ------------------------
# Generate plots
# ------------------------

unique(dt$utd_omr)
dt_helse <- dt %>% 
  filter(utd_omr=="Helsefag")  %>% 
  mutate(
    utd_type = str_remove(utd_type, "^Helsefag - "),
    utd_type = str_to_sentence(utd_type)
  )

unique(dt_helse$utd_type)

utd_values_helse <- unique(dt_helse$utd_type)
plots <- list()

folder_path <- "figurer/side_1_utdanningsomr/tabbed_utd_type_helse"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

for (utd in utd_values_helse) {
  
  dt_rs <- make_summary(dt_helse %>% filter(utd_type == utd))
  max_val <- max(dt_rs$value, na.rm = TRUE)
  if (max_val <= 0) max_val <- 1
  
  p <- ggplot(dt_rs, aes(aar, value, color = variable, group = variable)) +
    geom_line(size = 0.5) +
    geom_point(aes(text = paste(variable, ":", scales::comma(value))), size = 1.8) +
    scale_color_manual(values = hkdir_farger) +
    scale_y_continuous(limits = c(0, max_val * 1.05)) +
    scale_x_continuous(breaks = unique(dt_rs$aar)) +
    labs(title = paste("Utdanningstype:", utd), x = "", y = "") +
    theme_minimal(base_size = 11)
  
  plots[[utd]] <- ggplotly(p, tooltip = "text") %>%
    layout(
      height = 420,
      showlegend = FALSE   # ✅ disable Plotly legend
    ) %>%  
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
  
  plots[[utd]]$width  <- "100%"   # dynamisk bredde
  plots[[utd]]$height <- 320      # fast høyde 
  
  css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"

plots[[utd]] <- htmlwidgets::prependContent(
  plots[[utd]],
  htmltools::tags$style(htmltools::HTML(css))
)

saveWidget(
  plots[[utd]],
  file = file.path(folder_path, paste0(make_safe_filename(utd), ".html")),
  selfcontained = TRUE
)
}

# ------------------------
# Generate tabbed interface (iframes)
# ------------------------

tabs <- list()
tab_panels <- list()

for (i in seq_along(utd_values_helse)) {
  
  utd <- utd_values_helse[i]
  
  tabs[[i]] <- tags$li(
    class = ifelse(i == 1, "active", ""),
    tags$a(
      `data-toggle` = "tab",
      href = paste0("#tab", i),
      utd
    )
  )
  
  tab_panels[[i]] <- tags$div(
    class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
    id = paste0("tab", i),
    tags$iframe(
      src = paste0(make_safe_filename(utd), ".html"),
      style = "width:100%; height:420px; border:none;"
    )
  )
}

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "build_time", content = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), # For at Git skal registrere endring
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  tags$body(
    tags$div(
      class = "container",
      #tags$h2("Interaktive linjediagrammer per fagområde"),
      tags$ul(class = "nav nav-tabs", tabs),
      tags$div(class = "tab-content", tab_panels),
      shared_legend                 # ✅ shared legend here
    )
  )
)

save_html(page, file.path(folder_path, "tabbed_utd_type_helse.html"))




#################### UTDANNINGSTYPER - LÆRERUTDANNINGER ########################

# ------------------------
# Generate plots
# ------------------------

unique(dt$utd_omr)
dt_lærer <- dt %>% 
  filter(utd_omr=="Lærerutdanninger")  %>% 
  mutate(
    utd_type = str_remove(utd_type, "^Lærerutdanninger - "),
    utd_type = str_to_sentence(utd_type)
  )

unique(dt_lærer$utd_type)

utd_values_lærer <- unique(dt_lærer$utd_type)
plots <- list()

folder_path <- "figurer/side_1_utdanningsomr/tabbed_utd_type_helse_lærer"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

for (utd in utd_values_lærer) {
  
  dt_rs <- make_summary(dt_lærer %>% filter(utd_type == utd))
  max_val <- max(dt_rs$value, na.rm = TRUE)
  if (max_val <= 0) max_val <- 1
  
  p <- ggplot(dt_rs, aes(aar, value, color = variable, group = variable)) +
    geom_line(size = 0.5) +
    geom_point(aes(text = paste(variable, ":", scales::comma(value))), size = 1.8) +
    scale_color_manual(values = hkdir_farger) +
    scale_y_continuous(limits = c(0, max_val * 1.05)) +
    scale_x_continuous(breaks = unique(dt_rs$aar)) +
    labs(title = paste("Utdanningstype:", utd), x = "", y = "") +
    theme_minimal(base_size = 11)
  
  plots[[utd]] <- ggplotly(p, tooltip = "text") %>%
    layout(
      height = 420,
      showlegend = FALSE   # ✅ disable Plotly legend
    ) %>%  
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
  
  plots[[utd]]$width  <- "100%"   # dynamisk bredde
  plots[[utd]]$height <- 320      # fast høyde 
  
  css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"

plots[[utd]] <- htmlwidgets::prependContent(
  plots[[utd]],
  htmltools::tags$style(htmltools::HTML(css))
)

saveWidget(
  plots[[utd]],
  file = file.path(folder_path, paste0(make_safe_filename(utd), ".html")),
  selfcontained = TRUE
)
}

# ------------------------
# Generate tabbed interface (iframes)
# ------------------------

tabs <- list()
tab_panels <- list()

for (i in seq_along(utd_values_lærer)) {
  
  utd <- utd_values_lærer[i]
  
  tabs[[i]] <- tags$li(
    class = ifelse(i == 1, "active", ""),
    tags$a(
      `data-toggle` = "tab",
      href = paste0("#tab", i),
      utd
    )
  )
  
  tab_panels[[i]] <- tags$div(
    class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
    id = paste0("tab", i),
    tags$iframe(
      src = paste0(make_safe_filename(utd), ".html"),
      style = "width:100%; height:420px; border:none;"
    )
  )
}

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "build_time", content = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), # For at Git skal registrere endring
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  tags$body(
    tags$div(
      class = "container",
      #tags$h2("Interaktive linjediagrammer per fagområde"),
      tags$ul(class = "nav nav-tabs", tabs),
      tags$div(class = "tab-content", tab_panels),
      shared_legend                 # ✅ shared legend here
    )
  )
)

save_html(page, file.path(folder_path, "tabbed_utd_type_lærer.html"))




#################### UTDANNINGSTYPER - TEKNOLOGI ###############################

# ------------------------
# Generate plots
# ------------------------

unique(dt$utd_omr)
unique(dt$utd_type)

dt_tekno <- dt %>% 
  filter(utd_omr=="Teknologiske fag" & utd_type !="Økonomisk-administrative fag")  %>% 
  mutate(
    utd_type = str_remove(utd_type, "^Teknologiske fag - "),
    utd_type = str_to_sentence(utd_type)
  )

unique(dt_tekno$utd_type)

utd_values_tekno <- unique(dt_tekno$utd_type)
plots <- list()

folder_path <- "figurer/side_1_utdanningsomr/tabbed_utd_type_tekno"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

for (utd in utd_values_tekno) {
  
  dt_rs <- make_summary(dt_tekno %>% filter(utd_type == utd))
  max_val <- max(dt_rs$value, na.rm = TRUE)
  if (max_val <= 0) max_val <- 1
  
  p <- ggplot(dt_rs, aes(aar, value, color = variable, group = variable)) +
    geom_line(size = 0.5) +
    geom_point(aes(text = paste(variable, ":", scales::comma(value))), size = 1.8) +
    scale_color_manual(values = hkdir_farger) +
    scale_y_continuous(limits = c(0, max_val * 1.05)) +
    scale_x_continuous(breaks = unique(dt_rs$aar)) +
    labs(title = paste("Utdanningstype:", utd), x = "", y = "") +
    theme_minimal(base_size = 11)
  
  plots[[utd]] <- ggplotly(p, tooltip = "text") %>%
    layout(
      height = 420,
      showlegend = FALSE   # ✅ disable Plotly legend
    ) %>%  
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
  
  plots[[utd]]$width  <- "100%"   # dynamisk bredde
  plots[[utd]]$height <- 320      # fast høyde 
  
  css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"

plots[[utd]] <- htmlwidgets::prependContent(
  plots[[utd]],
  htmltools::tags$style(htmltools::HTML(css))
)

saveWidget(
  plots[[utd]],
  file = file.path(folder_path, paste0(make_safe_filename(utd), ".html")),
  selfcontained = TRUE
)
}

# ------------------------
# Generate tabbed interface (iframes)
# ------------------------

tabs <- list()
tab_panels <- list()

for (i in seq_along(utd_values_tekno)) {
  
  utd <- utd_values_tekno[i]
  
  tabs[[i]] <- tags$li(
    class = ifelse(i == 1, "active", ""),
    tags$a(
      `data-toggle` = "tab",
      href = paste0("#tab", i),
      utd
    )
  )
  
  tab_panels[[i]] <- tags$div(
    class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
    id = paste0("tab", i),
    tags$iframe(
      src = paste0(make_safe_filename(utd), ".html"),
      style = "width:100%; height:420px; border:none;"
    )
  )
}

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "build_time", content = format(Sys.time(), "%Y-%m-%d %H:%M:%S")), # For at Git skal registrere endring
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  tags$body(
    tags$div(
      class = "container",
      #tags$h2("Interaktive linjediagrammer per fagområde"),
      tags$ul(class = "nav nav-tabs", tabs),
      tags$div(class = "tab-content", tab_panels),
      shared_legend                 # ✅ shared legend here
    )
  )
)

save_html(page, file.path(folder_path, "tabbed_utd_type_tekno.html"))





################################################################
### KJØNNSFORDELING - STABLET STOLPEDIAGRAM (MED TABULERING) ###
################################################################


###### Stablet stolpediagram - kjønnsfordeling per utdanningsområde i år ######

utdomr_kjonn_data <- dt %>%
  filter(aar == siste_aar & prioritet==1) %>%
  group_by(utd_omr, kjonn) %>%
  summarise(n = n_distinct(fnr), .groups = "drop") %>%
  group_by(utd_omr) %>%
  mutate(
    percent = n / sum(n) * 100,  # Calculate % within each age group
    label = paste0(n, " (", round(percent, 1), "%)"),  # Format labels
    tooltip_text = paste0(
      "Utdanningsområde: ", utd_omr,
      "<br>Kjønn: ", kjonn,
      "<br>Søkere/andel: ", label
    )
  )
print(utdomr_kjonn_data)

# Sorterer dataene på andel menn

utdomr_kjonn_data <- utdomr_kjonn_data %>%
  mutate(utd_omr = factor(utd_omr, levels = utdomr_kjonn_data %>%
                            filter(kjonn == "Mann") %>%
                            arrange(percent) %>%
                            pull(utd_omr)))
print(utdomr_kjonn_data)

# Figur
utdomr_kjonn_stablet <- ggplot(utdomr_kjonn_data, aes(x = utd_omr, y = n, fill = kjonn, text = tooltip_text)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bars scaled to 100%
  geom_text(aes(label = label), 
            position = position_fill(vjust = 0.5), 
            size = 2.5, fontface = "bold", color = "white") +  # Labels inside bars
  scale_y_continuous(labels = percent) +  # Show % on y-axis
  scale_fill_manual(values = c("Kvinne" = "#E91E63", "Mann" = "#0D47A1"),
                    labels = c("Kvinne" = "Kvinner", "Mann" = "Menn")) +  # Custom colors
  labs(
    title = "",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) + 
  coord_flip()

utdomr_kjonn_stablet

# Convert ggplot -> plotly htmlwidget
utdomr_kjonn_stablet_w <- ggplotly(utdomr_kjonn_stablet, tooltip = "text") %>% 
  layout(
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.12,        # was -0.2 (too far down for short figure)
      font = list(size = 11)
    ),
    margin = list(l = 70, r = 20, t = 20, b = 60) # Her kan du justere venstremarginen
  )

utdomr_kjonn_stablet_w <- utdomr_kjonn_stablet_w %>% 
  config(
    displaylogo = TRUE,
    modeBarButtonsToRemove = c(
      "zoom2d", "pan2d", "select2d", "lasso2d",
      "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
      "hoverClosestCartesian", "hoverCompareCartesian"
    )
  )

utdomr_kjonn_stablet_w

# Save to file
out_path <- "figurer/side_1_utdanningsomr/utdomr_kjonn_stablet.html"
saveWidget(utdomr_kjonn_stablet_w, file = out_path, selfcontained = TRUE)





###### Stablet og tabulert stolpediagram - kjønnsfordeling per utdanningsområde over tid ######

# ------------------------
# Helper: safe filenames
# ------------------------
make_safe_filename <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^0-9A-Za-z_-]", "_", x)
}

# ------------------------
# Output folder
# ------------------------
folder_path <- "figurer/side_1_utdanningsomr/utdomr_kjonn_tabs/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

# (Optional) if you want aar as a factor without decimals but still ordered:
dt <- dt %>%
  mutate(aar = as.integer(aar)) %>%     # ensure numeric for sorting
  mutate(aar = factor(aar, levels = sort(unique(aar))))

utd_values <- sort(unique(dt$utd_omr))
plots <- list()

# ------------------------
# Generate plots + save each as HTML widget
# ------------------------
for (utd_kode in utd_values) {
  
  utdomr_kjonn_aar <- dt %>%
    filter(utd_omr == utd_kode, prioritet == 1) %>%
    group_by(aar, kjonn) %>%
    summarise(n = n_distinct(regnr), .groups = "drop") %>%
    group_by(aar) %>%
    mutate(
      percent = n / sum(n) * 100,
      label = paste0(n, "\n(", round(percent, 1), "%)"),
      tooltip_text = paste0(
        "År: ", aar,
        "<br>Kjønn: ", kjonn,
        "<br>Søkere/andel: ", label
      )
    ) %>%
    ungroup()
  
  # If a category has no data, skip (prevents empty html files / broken tabs)
  if (nrow(utdomr_kjonn_aar) == 0) next
  
  p <- ggplot(utdomr_kjonn_aar, aes(x = aar, y = n, fill = kjonn, text = tooltip_text)) +
    geom_col(position = "fill", width = 0.8) +
    geom_text(
      aes(label = label),
      position = position_fill(vjust = 0.5),
      size = 2.8, fontface = "bold", color = "white"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(
      values = c("Kvinne" = "#E91E63", "Mann" = "#0D47A1"),
      labels = c("Kvinne" = "Kvinner", "Mann" = "Menn")
    ) +
    labs(
      title = paste("Kjønnsfordeling førstevalgssøkere –", utd_kode),
      x = "", y = "", fill = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(face = "bold")
    )
  
  # Convert to htmlwidget
  w <- ggplotly(p, , tooltip = "text") %>%
    layout(height = 420, showlegend=FALSE) %>%  # Fjerner legend for enkeltfigurene (har delt legend)
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
  # Optional: responsive sizing CSS like in your other code
  css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"
w <- htmlwidgets::prependContent(
  w, htmltools::tags$style(htmltools::HTML(css))
)

# Save widget
file_name <- paste0(make_safe_filename(utd_kode), ".html")
saveWidget(w, file = file.path(folder_path, file_name), selfcontained = TRUE)

plots[[utd_kode]] <- file_name
}

# ------------------------
# Shared legend (HTML) – optional
# ------------------------
legend_items <- data.frame(
  label = c("Kvinner", "Menn"),
  color = c("#E91E63", "#0D47A1")
)

shared_legend <- tags$div(
  style = "
    display:flex;
    flex-wrap:wrap;
    gap:20px;
    margin-top:15px;
    font-size:14px;
  ",
  lapply(seq_len(nrow(legend_items)), function(i) {
    tags$div(
      style = "display:flex; align-items:center; gap:6px;",
      tags$span(style = paste0(
        "width:12px;height:12px;display:inline-block;background:",
        legend_items$color[i]
      )),
      tags$span(legend_items$label[i])
    )
  })
)

# ------------------------
# Build tabbed interface (iframes)
# ------------------------
tabs <- list()
tab_panels <- list()

valid_utd <- names(plots)  # only those actually saved (non-empty)

for (i in seq_along(valid_utd)) {
  
  utd <- valid_utd[i]
  src_file <- plots[[utd]]
  
  tabs[[i]] <- tags$li(
    class = ifelse(i == 1, "active", ""),
    tags$a(
      `data-toggle` = "tab",
      href = paste0("#tab", i),
      utd
    )
  )
  
  tab_panels[[i]] <- tags$div(
    class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
    id = paste0("tab", i),
    tags$iframe(
      src = src_file,  # relative to folder_path
      style = "width:100%; height:420px; border:none;"
    )
  )
}

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "build_time", content = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  tags$body(
    tags$div(
      class = "container",
      tags$ul(class = "nav nav-tabs", tabs),
      tags$div(class = "tab-content", tab_panels),
      shared_legend
    )
  )
)


htmltools::save_html(page, file.path(folder_path, "tabbed_utdomr_kjonn.html"))

##############################################################################################
### FØRSTEVALGSSØKERE OG PLANLAGTE STUDIEPLASSER - HORISONTALE STOLPEDIAGRAMMER (TABULERT) ###
##############################################################################################


### SIDELENGS STOLPEDIAGRAM - PLANLAGTE STUDIEPLASSER OG FØRSTEVALGSSØKERE ###


# ------------------------
# Helpers
# ------------------------
make_safe_filename <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^0-9A-Za-z_-]", "_", x)
}

# ------------------------
# Data prep (year = siste_aar)
# ------------------------
utdomr_studplass <- dt %>%
  filter(aar == siste_aar) %>%
  distinct(utd_omr, studiekode, studieplasser) %>%
  group_by(utd_omr) %>%
  summarise(studieplasser = sum(studieplasser, na.rm = TRUE), .groups = "drop")

utdomr_antallstud <- dt %>%
  filter(aar == siste_aar) %>%
  group_by(utd_omr) %>%
  summarise(studium = n_distinct(studiekode), .groups = "drop")

utdomr_forste <- dt %>%
  filter(aar == siste_aar, prioritet == 1) %>%
  group_by(utd_omr) %>%
  summarise(forstevalg = n_distinct(fnr), .groups = "drop")

utdomr_ferdig <- utdomr_antallstud %>%
  left_join(utdomr_studplass, by = "utd_omr") %>%
  left_join(utdomr_forste, by = "utd_omr") %>%
  mutate(
    forste_pr_plass = round(forstevalg / studieplasser, 1)
  )

# Keep "Total" if you want it elsewhere; plots use non-total only
utdomr_tilbar <- utdomr_ferdig %>%
  filter(utd_omr != "Total")

# ------------------------
# Plot factory (no geom_text; tooltip carries the value)
# ------------------------
bar_fun <- function(data, column, title, y_label, value_fmt = identity) {
  # Create a small plotting frame with standard column names
  plot_df <- data %>%
    transmute(
      utd_omr = utd_omr,
      value = .data[[column]],
      tooltip = paste0(
        "Utdanningsområde: ", utd_omr,
        "<br>", y_label, ": ", value_fmt(.data[[column]])
      )
    )
  
  ggplot(plot_df, aes(x = reorder(utd_omr, value), y = value, text = tooltip)) +
    geom_col(fill = "#E72F72") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "", y = y_label) +
    theme_minimal(base_size = 11)
}

s1 <- bar_fun(
  utdomr_tilbar,
  column = "studieplasser",
  title  = "Studieplasser per utdanningsområde",
  y_label = "Antall studieplasser",
  value_fmt = scales::comma
)

s2 <- bar_fun(
  utdomr_tilbar,
  column = "forstevalg",
  title  = "Førstevalg per utdanningsområde",
  y_label = "Antall førstevalg",
  value_fmt = scales::comma
)

s3 <- bar_fun(
  utdomr_tilbar,
  column = "forste_pr_plass",
  title  = "Førstevalg per studieplass",
  y_label = "Førstevalg per plass",
  value_fmt = function(x) format(x, nsmall = 1, trim = TRUE)
)

s1
s2
s3

# ------------------------
# Save as HTML widgets + build tabbed page
# ------------------------
folder_path <- "figurer/side_1_utdanningsomr/utdomr_bars_tabs/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

plots <- list(
  "Planlagte studieplasser" = s1,
  "Førstevalg" = s2,
  "Førstevalg per planlagte studieplass" = s3
)

# Export widgets
for (tab_title in names(plots)) {
  
  p <- plots[[tab_title]]
  
  w <- ggplotly(p, tooltip = "text") %>%
    layout(
      height = 520,
      showlegend = FALSE
    ) %>%
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
  
  # responsive sizing (optional)
  css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(420px, 60vh, 800px) !important;
}
"
w <- htmlwidgets::prependContent(
  w,
  htmltools::tags$style(htmltools::HTML(css))
)

file_name <- paste0(make_safe_filename(tab_title), ".html")
saveWidget(w, file = file.path(folder_path, file_name), selfcontained = TRUE)
}

# Build Bootstrap tabs that iframe the saved widgets
tabs <- list()
tab_panels <- list()
tab_titles <- names(plots)

for (i in seq_along(tab_titles)) {
  
  tab_title <- tab_titles[i]
  file_name <- paste0(make_safe_filename(tab_title), ".html")
  
  tabs[[i]] <- tags$li(
    class = ifelse(i == 1, "active", ""),
    tags$a(
      `data-toggle` = "tab",
      href = paste0("#tab", i),
      tab_title
    )
  )
  
  tab_panels[[i]] <- tags$div(
    class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
    id = paste0("tab", i),
    tags$iframe(
      src = file_name,
      style = "width:100%; height:560px; border:none;"
    )
  )
}

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "build_time", content = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  tags$body(
    tags$div(
      class = "container",
      tags$ul(class = "nav nav-tabs", tabs),
      tags$div(class = "tab-content", tab_panels)
    )
  )
)

htmltools::save_html(page, file.path(folder_path, "tabbed_utdomr_bars.html"))

