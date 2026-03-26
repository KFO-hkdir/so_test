
###################################
### UTENLANDSKE SØKERE UHG 2025 ###
###################################

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

library(DT)
library(tibble)


dt <- read.xlsx("data/utenlandske_sokere_uhg.xlsx")




## HKdirs farger

hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")


################################################################################
########## SØKERTALL FOR UTENLANDSKE SØKERE (UTDANNINGSBAKGRUNN) ###############
################################################################################



# ------------------------
# Helpers
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
folder_path <- "figurer/side_6_utenlandske/utenlandske"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

# Shared CSS (same pattern as your other widgets)
css <- "
.plotly.html-widget {
  width: 100% !important;
  height: clamp(320px, 60vh, 700px) !important;
}
"

# ============================================================
# 1) Line plot (previous one) -> widget
# ============================================================
utenlandske_aar <- tibble(
  aar = 2016:2025,
  sokere = c(3594, 3754, 4724, 4828, 4827, 5499, 4470, 4248, 4539, 4822)
) %>%
  mutate(
    tooltip_text = paste0(
      "År: ", aar,
      "<br>Søkere: ", scales::comma(sokere)
    )
  )

utenlandske_aar

source("funksjoner/linjediagram_funksjon.R")
source("funksjoner/sidelengs_stolpe_funksjon.R")

### Call the function ###
make_line_chart(
  data          = utenlandske_aar,
  x_var         = aar,
  series_vars   = c("sokere"),
  series_labels = c("Søkere med utenlandsk utdanning"),
  colors        = c(hkdir_farger[1]),
  title         = "Søkere med utenlandsk utdanning, 2016–2025",
  save_path     = "figurer/side_6_utenlandske_hc/linje/utenlandske_linje.html"
)


# ============================================================
# 2) Bar plot (Top 10 in 2025) -> widget
# ============================================================
dt_2025 <- dt %>%
  select(Land, `2025`) %>%
  rename(count = `2025`) %>%
  filter(!is.na(Land), !is.na(count)) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10) %>%
  mutate(
    tooltip_text = paste0(
      "Land: ", Land,
      "<br>Antall søkere (2025): ", scales::comma(count)
    )
  )

dt_2025


make_bar_chart(
  data         = dt_2025,
  category_var = Land,
  series_configs = list(
    list(value_col = "count", series_name = "Antall søkere", decimals = 0)
  ),
  color     = hkdir_farger[1],
  title = "Søkere med utenlandsk utdanningsbakgrunn",
  save_path = "figurer/side_6_utenlandske_hc/stolpe/utenlandske_topp10.html"
)





