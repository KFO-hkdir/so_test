

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

####################################################################################
### Linjediagram - førstevalgssøkere og planlagte studieplasser til nett/samling ###
####################################################################################


# ============================================================
# 1) Førstevalgssøkere: nett/samling vs sted
# ============================================================
forstesokere_aar <- dt %>%
  filter(prioritet == 1) %>%
  group_by(aar, fleksible) %>%
  summarise(n_sokere = n_distinct(regnr), .groups = "drop") %>%
  mutate(
    fleksible = case_when(
      fleksible == "J" ~ "Nettbasert eller samlingsbasert",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ),
    tooltip_text = paste0(
      "År: ", aar,
      "<br>Kategori: ", fleksible,
      "<br>Førstevalgssøkere: ", scales::comma(n_sokere)
    )
  )


# ============================================================
# 2) Planlagte studieplasser: nett/samling vs sted
# ============================================================
studplass_aar <- dt %>%
  group_by(aar, fleksible) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    fleksible = case_when(
      fleksible == "J" ~ "Nettbasert eller samlingsbasert",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ),
    tooltip_text = paste0(
      "År: ", aar,
      "<br>Kategori: ", fleksible,
      "<br>Planlagte studieplasser: ", scales::comma(antall_studieplasser)
    )
  )

forstesokere_aar

forstesokere_wide <- forstesokere_aar %>%
  select(aar, fleksible, n_sokere) %>%
  pivot_wider(names_from = fleksible, values_from = n_sokere)

studplass_wide <- studplass_aar %>%
  select(aar, fleksible, antall_studieplasser) %>%
  pivot_wider(names_from = fleksible, values_from = antall_studieplasser)

combined_line <- bind_rows(
  forstesokere_wide %>% mutate(metrikk = "Førstevalgssøkere"),
  studplass_wide   %>% mutate(metrikk = "Planlagte studieplasser")
)



source("funksjoner/linjediagram_funksjon.R")
make_line_chart(
  data           = combined_line,
  x_var          = aar,
  series_vars    = c("Stedbasert", "Nettbasert eller samlingsbasert"),
  series_labels  = c("Stedbasert", "Nettbasert eller samlingsbasert"),
  group_var      = metrikk,
  colors         = c("#E72F72", "#0025A0"),
  dropdown_label = "Vis:",
  save_path      = "figurer/side_3_nettogsamling_hc/linje/fleksible_linje_dropdown.html"
)



###########################################################################
### Andel førstevalgssøkere og planlagte studieplasser til nett/samling ###
###########################################################################

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(scales)


# ── 1. Build the two aggregations (keep only what the function needs) ─────────

forste_fleksible_tid <- dt %>%
  filter(prioritet == 1) %>%
  group_by(aar, fleksible) %>%
  summarise(n = n_distinct(regnr), .groups = "drop") %>%
  mutate(
    fleksible = case_when(
      fleksible == "J" ~ "Nettbasert eller samlingsbasert",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ),
    metric = "Førstevalgssøkere"
  )

planl_fleksible_tid <- dt %>%
  distinct(aar, fleksible, studiekode, studieplasser) %>%
  group_by(aar, fleksible) %>%
  summarise(n = sum(studieplasser, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    fleksible = case_when(
      fleksible == "J" ~ "Nettbasert eller samlingsbasert",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ),
    metric = "Planlagte studieplasser"
  )




forste_fleksible_tid <- dt %>%
  filter(prioritet == 1) %>%
  group_by(aar, fleksible) %>%
  summarise(n = n_distinct(regnr), .groups = "drop") %>%
  mutate(
    fleksible = case_when(
      fleksible == "J" ~ "Nettbasert eller samlingsbasert",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ),
    fleksible = factor(fleksible, levels = c("Stedbasert", "Nettbasert eller samlingsbasert")),
    metric = "Førstevalgssøkere"
  )

planl_fleksible_tid <- dt %>%
  distinct(aar, fleksible, studiekode, studieplasser) %>%
  group_by(aar, fleksible) %>%
  summarise(n = sum(studieplasser, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    fleksible = case_when(
      fleksible == "J" ~ "Nettbasert eller samlingsbasert",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ),
    fleksible = factor(fleksible, levels = c("Stedbasert", "Nettbasert eller samlingsbasert")),
    metric = "Planlagte studieplasser"
  )


# ── 2. Combine ────────────────────────────────────────────────────────────────

fleksible_stacked <- bind_rows(forste_fleksible_tid, planl_fleksible_tid)

# ── 3. Call the function ──────────────────────────────────────────────────────
source("funksjoner/stablet_stolpe_dropdown_funksjon.R")

make_stacked_pct_dropdown(
  data           = fleksible_stacked,
  category_var   = aar,
  stack_var      = fleksible,
  value_var      = n,
  group_var      = metric,
  colors         = c(
    "Stedbasert"                      = "#0025A0",
    "Nettbasert eller samlingsbasert" = "#E72F72"
  ),
  orientation    = "vertical",
  chart_height   = 450,
  bar_width      = 40,
  cat_label      = "År",
  stack_label    = "Kategori",
  dropdown_label = "Vis:",
  save_path      = "figurer/side_3_nettogsamling_hc/stolpe/andel_fleksible_aar.html"
)






#################################################################
### Fordeling nett/samling og stedbasert på utdanningsområder ### 
#################################################################

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(scales)




fleksible_sokere_utdomr <- dt %>%
  filter(aar == siste_aar, prioritet == 1) %>%
  group_by(utd_omr, fleksible) %>%
  summarise(n = n_distinct(regnr), .groups = "drop") %>%
  mutate(
    fleksible = factor(case_when(
      fleksible == "J" ~ "Nett/samling",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ), levels = c("Stedbasert", "Nett/samling")),
    metric = "Førstevalgssøkere"
  )

fleksible_planl_utdomr <- dt %>%
  filter(aar == siste_aar) %>%
  distinct(utd_omr, fleksible, studiekode, studieplasser) %>%
  group_by(utd_omr, fleksible) %>%
  summarise(n = sum(studieplasser, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    fleksible = factor(case_when(
      fleksible == "J" ~ "Nett/samling",
      fleksible == "N" ~ "Stedbasert",
      TRUE ~ as.character(fleksible)
    ), levels = c("Stedbasert", "Nett/samling")),
    metric = "Planlagte studieplasser"
  )

fleksible_utdomr <- bind_rows(fleksible_sokere_utdomr, fleksible_planl_utdomr)

source("funksjoner/stablet_stolpe_dropdown_funksjon.R")

make_stacked_pct_dropdown(
  data           = fleksible_utdomr,
  category_var   = utd_omr,
  stack_var      = fleksible,
  value_var      = n,
  group_var      = metric,
  order_by       = "Stedbasert",
  colors         = c(
    "Stedbasert"   = "#0025A0",
    "Nett/samling" = "#E72F72"
  ),
  orientation    = "horizontal",
  chart_height   = 520,
  bar_width      = 26,
  cat_label      = "Utdanningsområde",
  stack_label    = "Kategori",
  dropdown_label = "Vis:",
  save_path      = "figurer/side_3_nettogsamling_hc/stolpe/andel_fleksible_utdomr.html"
)







