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


dt <- read_fst("data/so_sokertall_master_red.fst")
dt_bsp <- read_xlsx("data/statsborgerprøven_resultater_data.xlsx")

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


#################### SJEKKER VARIABLENE I BSP DATASETTET #######################


dt_bsp %>%  
  group_by(Karakter) %>% 
  summarise(n = n_distinct(Kandidat)) %>% 
  mutate(
    percentage_num = 100 * n / sum(n),
    percentage = scales::number(percentage_num, accuracy = 0.1, decimal.mark = ","),
    percentage = paste0(percentage, " %")
  ) 

dt_bsp %>%  
  group_by(Fylke) %>% 
  summarise(n = n_distinct(Kandidat)) %>% 
  mutate(
    percentage_num = 100 * n / sum(n),
    percentage = scales::number(percentage_num, accuracy = 0.1, decimal.mark = ","),
    percentage = paste0(percentage, " %")
  ) 

## Lager variabler og tooltip
dt_bsp_tooltip <- dt_bsp %>%
  group_by(Fylke) %>%
  summarise(
    n_applicants = n_distinct(Kandidat),
    n_bestatt = n_distinct(Kandidat[Karakter == "BESTATT"]),
    n_ikkebestatt = n_distinct(Kandidat[Karakter == "IKKEBESTATT"]),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = scales::number(
      (100 * n_bestatt / n_applicants),
      accuracy = 0.1,
      decimal.mark = ","
    ),
    percentage = paste0(percentage, " %")
  )

dt_bsp_tooltip


## Lager variabler og data til kommunetabeller
dt_bsp_tabelldata <- dt_bsp %>%
  group_by(Fylke,Kommune) %>%
  summarise(
    n_applicants = n_distinct(Kandidat),
    n_bestatt = n_distinct(Kandidat[Karakter == "BESTATT"]),
    n_ikkebestatt = n_distinct(Kandidat[Karakter == "IKKEBESTATT"]),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = scales::number(
      (100 * n_bestatt / n_applicants),
      accuracy = 0.1,
      decimal.mark = ","
    ),
    percentage = paste0(percentage, " %")
  ) %>% # Anonymiserer
  mutate(
    n_bestatt     = if_else(n_applicants < 10, NA_integer_, n_bestatt),
    n_ikkebestatt = if_else(n_applicants < 10, NA_integer_, n_ikkebestatt),
    percentage    = if_else(n_applicants < 10, NA_character_, percentage)
  ) 


dt_bsp_tabelldata



############################################
### KART - FORBEREDER DATASETT M/TOOLTIP ###
############################################

### BRUK DENNE? VINNER! 

### KART - OPPSETT ###


# Hente navn på fylker
county_names <- data.frame(
  location_code = c("county_nor01", "county_nor02", "county_nor03", "county_nor11", "county_nor15",
                    "county_nor18", "county_nor33", "county_nor34", "county_nor39", "county_nor40",
                    "county_nor42", "county_nor46", "county_nor50", "county_nor55", "county_nor56"),
  location_name = c("Østfold", "Akershus", "Oslo", "Rogaland", "Møre og Romsdal",
                    "Nordland", "Buskerud", "Innlandet", "Vestfold", "Telemark",
                    "Agder", "Vestland", "Trøndelag", "Troms", "Finnmark")
)


# View mapping
print(county_names)


# 🎯 Use a high-resolution dataset instead of `map_df`
map_sf <- csmaps::nor_county_map_b2024_default_sf %>%
  left_join(county_names, by = "location_code") %>%
  left_join(dt_bsp_tooltip, by = c("location_name" = "Fylke"))

map_sf
unique(map_sf$location_name)
# 🌍 Define color palette
pal <- colorFactor(RColorBrewer::brewer.pal(n = 8, "Dark2"), domain = map_sf$location_name)


# Legger til url for tabeller i map_sf

make_safe_filename <- function(x) {
  x <- trimws(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^0-9A-Za-z_-]", "_", x)
  tolower(x)
}

bsp_base_url <- "https://kfo-hkdir.github.io/so_test/kart/bsp_tabeller/"

map_sf <- map_sf %>%
  mutate(
    table_url = paste0(bsp_base_url, make_safe_filename(location_name), ".html")
  )

########################################################################
### LAGER KART - VERSJON MED JUSTERING UAVHENGIG AV IFRAME-STØRRELSE ###
########################################################################

# 🗺️ Create a clean, high-quality interactive map
kart_bsp_2025 <- leaflet(
  map_sf,
  height = 800,
  options = leafletOptions(
    zoomControl = FALSE,
    dragging = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    boxZoom = FALSE,
    keyboard = FALSE,
    touchZoom = FALSE,
    tap = FALSE
  )
) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lng = 18, lat = 65, zoom = 5) %>%
  addPolygons(
    fillColor = "#4A68F0",
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    smoothFactor = 0.5,
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    
    # IMPORTANT: store the URL here
    layerId = ~table_url,
    
    # Keep your hover tooltip as info only (not clickable)
    label = ~paste0(
      "<b style='font-size:14px;'>", location_name, "</b><br>",
      "<span style='font-size:12px;'>Antall kandidater: ", n_applicants, "</span><br>",
      "<span style='font-size:12px;'>Bestått: ", n_bestatt, "</span><br>",
      "<span style='font-size:12px;'>Ikke bestått: ", n_ikkebestatt, "</span><br>",
      "<span style='font-size:12px;'>Andel bestått: ", percentage, "</span><br>",
      "<span style='font-size:12px; color:#1a73e8;'>Klikk fylket for tabell</span>"
    ) %>% lapply(htmltools::HTML),
    
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "color" = "black"),
      direction = "auto"
    )
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;

      function bindClicks() {
        map.eachLayer(function(layer) {
          // only paths (polygons) that have a layerId
          if (layer && layer.options && layer.options.layerId && layer instanceof L.Path) {

            // pointer cursor
            layer.on('mouseover', function() {
              if (layer._path) layer._path.style.cursor = 'pointer';
            });

            // open the URL stored in layerId
            layer.on('click', function(e) {
              var url = layer.options.layerId;
              if (url) window.open(url, '_blank');
            });
          }
        });
      }

      bindClicks();
    }
  ")

kart_bsp_2025

folder_path <- "kart/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

htmlwidgets::saveWidget(
  widget = kart_bsp_2025,
  file = file.path(folder_path, "kart_bsp.html"),
  selfcontained = TRUE
)





### LAGER DATATABLES MED TALL FOR HVER KOMMUNE I HVERT FYLKE ###

make_safe_filename <- function(x) {
  x <- trimws(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^0-9A-Za-z_-]", "_", x)
  tolower(x)
}

create_gt_table <- function(fylke_name, data) {
  
  tab <- data %>%
    filter(Fylke == fylke_name) %>%
    arrange(desc(n_applicants)) %>%
    select(Kommune, n_applicants, n_bestatt, n_ikkebestatt, percentage) %>%
    gt() %>%
    tab_header(title = md(paste0("**Kommuner i ", fylke_name, "**"))) %>%
    cols_label(
      Kommune = "Studiested",
      n_applicants   = "Antall kandidater",
      n_bestatt = "Bestått",
      n_ikkebestatt = "Ikke bestått",
      percentage     = "Andel bestått"
    ) %>%
    fmt_number(columns = n_applicants, decimals = 0, sep_mark = " ") %>%
    cols_align(align = "left",  columns = Kommune) %>%
    cols_align(align = "right", columns = c(n_applicants, n_bestatt, n_ikkebestatt, percentage)) %>%
    cols_width(
      Kommune ~ px(130),
      n_applicants   ~ px(70),
      n_bestatt ~ px(70),
      n_ikkebestatt ~ px(70),
      percentage     ~ px(70)
    ) %>%
    tab_options(
      table.width = pct(100),
      data_row.padding = px(4),
      table.font.size = px(12),
      heading.title.font.size = px(14),
      column_labels.font.weight = "bold"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#F58220"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>%
    opt_row_striping() %>%
    tab_options(row.striping.background_color = "#fde9d9")
  
  # Force wrapping for long place names + no overflow
  tab <- tab %>%
    tab_style(
      style = cell_text(whitespace = "normal"),
      locations = cells_body(columns = Kommune)
    )
  
  tab
}


# Tester funksjonen
create_gt_table("Agder", dt_bsp_tabelldata)

### LAGER OG LAGRER TABELLENE ###

folder_path <- "kart/bsp_tabeller/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

fylker <- sort(unique(dt_bsp_tabelldata$Fylke))

# Save table HTMLs
for (f in fylker) {
  gt_tbl <- create_gt_table(f, dt_bsp_tabelldata)
  
  # Wrap in a minimal HTML page with a max width ~640px
  page_tbl <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$style(HTML("
        body { margin: 0; padding: 0; font-family: sans-serif; }
        .wrap { max-width: 640px; margin: 0 auto; padding: 8px; }
        table { width: 100% !important; }
      "))
    ),
    tags$body(
      tags$div(class = "wrap", gt::as_raw_html(gt_tbl) |> HTML())
    )
  )
  
  htmltools::save_html(page_tbl, file = file.path(folder_path, paste0(make_safe_filename(f), ".html")))
}