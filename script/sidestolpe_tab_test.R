#####################################################
### Tester tabulering av sidelengs  stolpediagram ###
#####################################################
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
library(forcats)

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
