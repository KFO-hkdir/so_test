
##################################################
### Tester tabulering av stablet stolpediagram ###
##################################################
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
      style = "width:100%; height:480px; border:none;"
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