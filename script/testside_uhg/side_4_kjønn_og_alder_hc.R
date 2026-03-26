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
library(sf)
library(csmaps)
library(leaflet)
library(readr)
library(csdata)
library(ggtext)
library(ggmap)
library(scico)
library(fst)
library(RColorBrewer)
library(patchwork)
library(glue)

dt <- read_fst("data/so_sokertall_master_red.fst")

# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)


### Lager variablene studiested_fylke og fylke_soker ###

dt <- dt %>%
  rename(fylke_soker = fylke,
         studiested_fylke = studsted_fylke)



################################################################################
######################## ALDER #################################################
################################################################################



### Lager aldersfordeling 
dt <- dt %>%
  mutate(alder_gruppert = case_when(
    alder >= 16 & alder <= 19 ~ "16-19",
    alder >= 20 & alder <= 21 ~ "20-21",
    alder >= 22 & alder <= 24 ~ "22-24",
    alder >= 25 & alder <= 29 ~ "25-29",
    alder >= 30 & alder <= 34 ~ "30-34",
    alder >= 35 ~ "35+",
    TRUE ~ NA_character_  # Assigns NA to missing or invalid values
  ))


###############################
### Alder - noen nøkkeltall ###
###############################

dt %>% 
  summarise(snitt = mean(na.omit(alder)))



######################
### LAGER FIGURENE ###
######################


# ------------------------
# Helper: safe filenames
# ------------------------
make_safe_filename <- function(x) {
  x <- trimws(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^0-9A-Za-z_-]", "_", x)
  tolower(x)
}

# ------------------------
# Output folder
# ------------------------
folder_path <- "figurer/side_4_kjønn_og_alder/alder_tabs/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

# Shared CSS (same idea as your other widgets/tables)
css_widget <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"

css_table_page <- "
body { margin: 0; padding: 0; font-family: sans-serif; }
.wrap { max-width: 640px; margin: 0 auto; padding: 8px; }
table { width: 100% !important; }
"

apply_plotly_config <- function(p) {
  p %>%
    config(
      displaylogo = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d","pan2d","select2d","lasso2d",
        "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
        "hoverClosestCartesian","hoverCompareCartesian"
      )
    )
}

# ============================================================
# TABLE 1: Snittalder per utdanningsområde (prioritet==1) + Totalt
# ============================================================

years <- 2015:2025

snittalder_utdomr <- dt %>%
  filter(prioritet == 1, !is.na(alder_gruppert)) %>%
  mutate(aar = factor(aar, levels = years)) %>%
  group_by(aar, utd_omr) %>%
  summarise(snittalder = round(mean(alder, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = aar, values_from = snittalder) %>%
  arrange(desc(`2024`))

totalt_row <- dt %>%
  filter(prioritet == 1) %>%
  mutate(aar = factor(aar, levels = years)) %>%
  group_by(aar) %>%
  summarise(snittalder = round(mean(alder, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = aar, values_from = snittalder) %>%
  mutate(utd_omr = "Totalt") %>%
  select(utd_omr, everything())

snittalder_utdomr_med_totalt <- bind_rows(snittalder_utdomr, totalt_row)

snittalder_utdomr_med_totalt

source("funksjoner/tabell_funksjon.R")


make_hc_table(
  data = snittalder_utdomr_med_totalt,
  display_cols = c(
    "Utdanningsområde" = "utd_omr",
    "2019" = "2019",
    "2020" = "2020",
    "2021" = "2021",
    "2022" = "2022",
    "2023" = "2023",
    "2024" = "2024",
    "2025" = "2025"
  ),
  sort_by = "2024",
  sort_desc = FALSE,
  title = "Gjennomsnittsalder per utdanningsområde",
  size = "normal",
  first_col_width = "240px",
  export_xlsx = TRUE,
  export_file_stem = "alder_utdanningsomraade",
  out_file = "figurer/side_4_kjønn_og_alder_hc/tabeller/alder_snitt_table.html"
)



# ============================================================
# TABLE 2: Antall søkere (prioritet==1) per aldersgruppe x år
# ============================================================

alder_tilbud_pivot <- dt %>%
  filter(!is.na(alder_gruppert)) %>% 
  group_by(aar, alder_gruppert) %>%
  summarise(sokere = n_distinct(regnr), .groups = "drop") %>%
  mutate(aar = factor(aar, levels = years)) %>%
  pivot_wider(names_from = aar, values_from = sokere) %>%
  arrange(alder_gruppert)

gt_alder_tab <- alder_tilbud_pivot %>%
  gt() %>%
  tab_header(
    title = md("**Antall søkere per aldersgruppe  (førstevalg)**")
  ) %>%
  cols_label(alder_gruppert = "Aldersgruppe") %>%
  fmt_number(columns = -alder_gruppert, decimals = 0, sep_mark = " ") %>%
  cols_align(align = "left", columns = alder_gruppert) %>%
  cols_align(align = "right", columns = -alder_gruppert) %>%
  cols_width(
    alder_gruppert ~ px(50),
    everything() ~ px(45)
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#F58220"), cell_text(color = "white", weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(4),
    table.font.size = px(11),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11)
  ) %>%
  tab_style(
    style = cell_text(whitespace = "normal"),
    locations = cells_body(columns = alder_gruppert)
  )

gt_alder_tab

page_tbl2 <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$style(HTML(css_table_page))
  ),
  tags$body(
    tags$div(class = "wrap", gt::as_raw_html(gt_alder_tab) |> HTML())
  )
)

htmltools::save_html(page_tbl2, file.path(folder_path, "aldergruppe_tabell.html"))



# ============================================================
# PLOT: Aldersfordeling per år (stacked 100%) – widget
# (No need for a function since it’s made once)
# ============================================================

dt_tilb <- dt %>%
  filter(prioritet == 1, !is.na(alder_gruppert)) %>%
  group_by(aar, alder_gruppert) %>%
  summarise(sokere = n_distinct(regnr), .groups = "drop") %>%
  group_by(aar) %>%
  mutate(
    andel = sokere / sum(sokere),
    percent = andel * 100,
    label = if_else(
      percent >= 8,
      paste0(scales::number(sokere, big.mark = " "), "\n", round(percent, 1), "%"),
      ""
    ),
    tooltip_text = paste0(
      "År: ", aar,
      "<br>Aldersgruppe: ", alder_gruppert,
      "<br>Antall: ", scales::number(sokere, big.mark = " "),
      "<br>Andel: ", scales::percent(andel, accuracy = 0.1)
    )
  ) %>%
  ungroup()

p_age <- ggplot(dt_tilb, aes(x = aar, y = andel, fill = alder_gruppert, text = tooltip_text)) +
  geom_col(position = "fill", width = 0.8) +
  geom_text(
    aes(label = label),
    position = position_fill(vjust = 0.5),
    size = 2.3, fontface = "bold", color = "white",
    lineheight = 0.95
  ) +
  scale_x_continuous(breaks = sort(unique(dt_tilb$aar))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = hkdir_farger) +
  labs(
    title = "Aldersfordeling søkere per år",
    x = "", y = "", fill = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

w_age <- ggplotly(p_age, tooltip = "text", responsive = TRUE) %>%
  layout(
    hovermode = "closest",
    margin = list(l = 50, r = 20, t = 35, b = 60),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
  ) %>%
  apply_plotly_config()

w_age$width  <- "100%"
w_age$height <- 520
w_age <- htmlwidgets::prependContent(w_age, tags$style(HTML(css_widget)))

w_age

saveWidget(w_age, file = file.path(folder_path, "aldersfordeling.html"), selfcontained = TRUE)


# # ============================================================
# # TAB PAGE: 2 tables + 1 plot
# # ============================================================
# 
# tab_titles <- c(
#   "Gjennomsnittsalder (utd.omr)",
#   "Antall per aldersgruppe",
#   "Aldersfordeling (figur)"
# )
# 
# tab_files <- c(
#   "snittalder_utdomr.html",
#   "aldergruppe_tabell.html",
#   "aldersfordeling.html"
# )
# 
# tabs <- list()
# tab_panels <- list()
# 
# for (i in seq_along(tab_titles)) {
#   tabs[[i]] <- tags$li(
#     class = ifelse(i == 1, "active", ""),
#     tags$a(`data-toggle` = "tab", href = paste0("#tab", i), tab_titles[i])
#   )
#   
#   tab_panels[[i]] <- tags$div(
#     class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
#     id = paste0("tab", i),
#     tags$iframe(
#       src = tab_files[i],
#       style = "width:100%; max-width:640px; height:560px; border:none;"
#     )
#   )
# }
# 
# page <- tags$html(
#   tags$head(
#     tags$meta(charset = "utf-8"),
#     tags$meta(name = "build_time", content = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
#     tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"),
#     tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
#     tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js"),
#     tags$style(HTML("
#       .container { max-width: 700px; }
#       .nav-tabs { margin-bottom: 10px; }
#     "))
#   ),
#   tags$body(
#     tags$div(
#       class = "container",
#       tags$ul(class = "nav nav-tabs", tabs),
#       tags$div(class = "tab-content", tab_panels)
#     )
#   )
# )
# 
# htmltools::save_html(page, file.path(folder_path, "tabbed_alder.html"))



################################################################################
######################### KJØNN ################################################
################################################################################


source("funksjoner/stablet_stolpe_funksjon.R")

### KJØNNSFORDELING FØRSTEVALGSSØKERE PER UTDANNINGSOMRÅDE ###

# ── Prepare data (same as your ggplot pipeline) ────────────────────────────
utdomr_kjonn_data <- dt %>%
  filter(aar == siste_aar, prioritet == 1) %>%
  group_by(utd_omr, kjonn) %>%
  summarise(n = n_distinct(regnr), .groups = "drop")


# ── Call the function ──────────────────────────────────────────────────────
utdomr_kjonn_hc <- make_stacked_pct_chart(
  data         = utdomr_kjonn_data,
  category_var = utd_omr,
  stack_var    = kjonn,
  value_var    = n,
  cat_label = "Utdanningsområde",
  stack_label = "Kjønn",
  order_by     = "Mann",          # sort bars ascending by male share
  colors       = c(
    "Mann"   = "#0D47A1",
    "Kvinne" = "#E91E63"
  ),
  orientation  = "horizontal",
  chart_height = 700,
  bar_width    = 24,
  label_font_size = "9px",
  title        = "Kjønnsfordeling førstevalgssøkere per utdanningsområde",
  save_path    = "figurer/side_4_kjønn_og_alder_hc/stolpediagram/utdomr_kjonn_stablet.html"
)

utdomr_kjonn_hc



### KJØNNSFORDELING FØRSTEVALGSSØKERE OVER TID ###


# Vertical chart example – region by landsdel
aar_kjonn_data <- dt %>%
  group_by(aar, kjonn) %>%
  summarise(n = n_distinct(regnr), .groups = "drop")

aar_kjonn_hc <- make_stacked_pct_chart(
  data         = aar_kjonn_data,
  category_var = aar,
  stack_var    = kjonn,
  value_var    = n,
  order_by     = NULL,          # years should stay in chronological order
  colors       = c(
    "Mann"   = "#0D47A1",
    "Kvinne" = "#E91E63"
  ),
  orientation  = "vertical",
  chart_height = 620,
  bar_width    = 60,            # columns can be a bit wider than horizontal bars
  label_font_size = "11px",
  title        = "Kjønnsfordeling søkere per år",
  save_path    = file.path("figurer/side_4_kjønn_og_alder_hc/stolpediagram/alle_sokere.html")
)

aar_kjonn_hc



### KJØNSSFORDELING PER UTDANNINGSOMRÅDE - MED NEDTREKKSLISTE ###

### Funksjon med arguemnt for dropdown-liste - FUNKER!!!
source("funksjoner/stablet_stolpe_dropdown_funksjon.R")


dt2 <- dt %>% mutate(aar = as.integer(aar))

# Prepare summarised data for all utd_omr groups at once
utdomr_kjonn_aar <- dt2 %>%
  filter(prioritet == 1) %>%
  group_by(utd_omr, aar, kjonn) %>%
  summarise(n = n_distinct(regnr), .groups = "drop")

# Dropdown
make_stacked_pct_dropdown(
  data           = utdomr_kjonn_aar,
  category_var   = aar,
  stack_var      = kjonn,
  value_var      = n,
  group_var      = utd_omr,
  colors         = c("Mann" = "#0D47A1", "Kvinne" = "#E91E63"),
  orientation    = "vertical",
  chart_height   = 520,
  bar_width      = 60,
  cat_label      = "År",
  stack_label    = "Kjønn",
  dropdown_label = "Utdanningsområde:",
  save_path      = "figurer/side_4_kjønn_og_alder_hc/stolpediagram/kjonn_utd_omr_dropdown.html"
)
