##########################
### TESTER HIGHCHARTER ###
##########################

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

library(highcharter)
library(jsonlite)

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


##############################
### NASJONALT LINJEDIAGRAM ###
##############################
source("funksjoner/linjediagram_funksjon.R")
### Data prep (unchanged) ###
sokere_aar <- dt %>%
  group_by(aar) %>%
  summarise(n_sokere = n_distinct(regnr))

studplass_aar <- dt %>%
  group_by(aar) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE))

dt_all <- sokere_aar %>%
  left_join(studplass_aar, by = "aar")

### Call the function ###
make_line_chart(
  data          = dt_all,
  x_var         = aar,
  series_vars   = c("n_sokere", "antall_studieplasser"),
  series_labels = c("Søkere", "Studieplasser"),
  colors        = c(hkdir_farger[1], hkdir_farger[2]),
  save_path     = "figurer/side_1_utdanningsomr_hc/linje/nasjonalt_linje.html"
)

#########################################################
### LINJEDIAGRAM I NEDTREKKSLISTE - UTDANNINGSOMRÅDER ###
#########################################################

source("funksjoner/linjediagram_funksjon.R")

### Data prep ###
sokere <- dt %>%
  filter(prioritet == 1) %>%
  group_by(utd_omr, aar) %>%
  summarise(n_sokere = n_distinct(regnr), .groups = "drop")

studplass <- dt %>%
  filter(prioritet == 1) %>%
  distinct(utd_omr, aar, studiekode, studieplasser) %>%
  group_by(utd_omr, aar) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE), .groups = "drop")


charts_df <- sokere %>%
  left_join(studplass, by = c("utd_omr", "aar"))



### Call the function ###
make_line_chart(
  data           = charts_df,   # your pre-aggregated data frame
  x_var          = aar,
  series_vars    = c("n_sokere", "antall_studieplasser"),
  series_labels  = c("Søkere", "Studieplasser"),
  group_var      = utd_omr,
  colors         = c(hkdir_farger[1], hkdir_farger[2]),
  dropdown_label = "Utdanningsområde:",
  save_path      = "figurer/side_1_utdanningsomr_hc/linje/utdomr_linje_dropdown.html"
)



#######################################################
### LINJEDIAGRAM I NEDTREKKSLISTE - UTDANNINGSTYPER ###
#######################################################

source("funksjoner/linjediagram_funksjon.R")

# ── Data prep ─────────────────────────────────────────────────────────────────
omr_values <- c("Helsefag", "Lærerutdanninger", "Teknologiske fag")

studplass <- dt %>%
  filter(prioritet == 1, utd_omr %in% omr_values) %>%
  distinct(utd_omr, utd_type, aar, studiekode, studieplasser) %>%
  group_by(utd_omr, utd_type, aar) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE), .groups = "drop")

sokere <- dt %>%
  filter(prioritet == 1, utd_omr %in% omr_values) %>%
  group_by(utd_omr, utd_type, aar) %>%
  summarise(n_sokere = n_distinct(regnr), .groups = "drop")

charts_df <- sokere %>%
  left_join(studplass, by = c("utd_omr", "utd_type", "aar"))

# ── Remove anomalies ──────────────────────────────────────────────────────────
# 1. Drop any utd_type ending in "- annet" (case-insensitive) across all omr_values
# 2. Drop the misclassified "Økonomisk-administrative fag" row under "Teknologiske fag"
charts_df <- charts_df %>%
  filter(!grepl("- annet$", utd_type, ignore.case = TRUE)) %>%
  filter(!(utd_omr == "Teknologiske fag" & utd_type == "Økonomisk-administrative fag"))

# ── Strip the "Helsefag - " etc. prefix from utd_type ────────────────────────
charts_df <- charts_df %>%
  mutate(
    utd_type_label = mapply(function(omr, type) {
      stripped <- sub(paste0(omr, " - "), "", type)
      paste0(toupper(substr(stripped, 1, 1)), substr(stripped, 2, nchar(stripped)))
    }, utd_omr, utd_type)
  )

# ── One file per utd_omr, same filenames as inline script ─────────────────────
folder_path <- "figurer/side_1_utdanningsomr_hc/linje"

for (omr in omr_values) {
  clean_name <- tolower(gsub(" ", "_", omr))
  
  make_line_chart(
    data           = charts_df %>% filter(utd_omr == omr),
    x_var          = aar,
    series_vars    = c("n_sokere", "antall_studieplasser"),
    series_labels  = c("Søkere", "Studieplasser"),
    group_var      = utd_type_label,        # uses the cleaned label column as the dropdown
    sort_groups = FALSE,
    colors         = c(hkdir_farger[1], hkdir_farger[2]),
    dropdown_label = "Utdanningstype:",
    save_path      = file.path(folder_path, paste0("utdomr_linje_dropdown_", clean_name, ".html"))
  )
}


################################################
### SIDELENGS STOPLEDIAGRAM I NEDTREKKSLISTE ###
################################################

source("funksjoner/sidelengs_stolpe_funksjon.R")


# ── 1. Data prep ──────────────────────────────────────────────────────────────
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
  left_join(utdomr_forste,    by = "utd_omr") %>%
  mutate(forste_pr_plass = round(forstevalg / studieplasser, 1)) %>%
  filter(utd_omr != "Total")


### Call the function 
make_bar_chart(
  data         = utdomr_ferdig,
  category_var = utd_omr,
  series_configs = list(
    list(value_col = "studieplasser",   series_name = "Studieplasser",        dropdown_label = "Planlagte studieplasser",             decimals = 0),
    list(value_col = "forstevalg",      series_name = "Førstevalg",           dropdown_label = "Førstevalg",                          decimals = 0),
    list(value_col = "forste_pr_plass", series_name = "Førstevalg per plass", dropdown_label = "Førstevalg per planlagte studieplass", decimals = 1)
  ),
  color     = hkdir_farger[1],
  save_path = "figurer/side_1_utdanningsomr_hc/stolpe/utdomr_bars_dropdown.html"
)


### Eksempel med enkeltfigur
make_bar_chart(
  data         = utdomr_ferdig,
  category_var = utd_omr,
  series_configs = list(
    list(value_col = "forste_pr_plass", series_name = "Førstevalg per plass", decimals = 1)
  ),
  color     = hkdir_farger[1],
  save_path = "figurer/side_1_utdanningsomr_hc/stolpe/utdomr_førsteperplass_bar.html"
)