
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


dt <- read_fst("data/so_sokertall_master_red.fst")

dt2 <- read_csv2("data/SO_poenggrense_grunnfil_UHG_25_ny.csv")
names(dt2)
names(dt2) <- c("kvote","inst_navn",
               "inst_navn2","opptaksrunde",
               "poenggrense","poenggrense2",
               "sesong","studiekode", "studiepoeng",
               "studiested","programnavn",
               "tid","utd_type",
               "aar","venteliste")

dt <- dt %>%
  left_join(
    dt2 %>% distinct(inst_navn, inst_navn2),
    by = "inst_navn"
  )

dt %>%
  filter(aar==2025) %>% 
  distinct(inst_navn, inst_navn2) %>%
  arrange(inst_navn)

# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)


### POPULÆRE STUDIER ### 
populære_min <- dt %>% 
  filter(aar == siste_aar & prioritet==1) %>% 
  group_by(inst_navn2, programnavn,utd_type, studiekode) %>% 
  summarise(
    førstev = n_distinct(fnr),
    studieplasser = unique(studieplasser)
  ) %>%  
  mutate(
    søker_pr_plass = round((førstev/studieplasser), 1)
  ) %>% 
  arrange(desc(søker_pr_plass))

populære_min


populære_min <- populære_min %>% 
  select("inst_navn2","programnavn", "utd_type", "søker_pr_plass")

populære_min

source("funksjoner/tabell_funksjon_stor.R")

make_hc_table_l(
  data = populære_min,
  display_cols = c(
    "Institusjon"       = "inst_navn2",
    "Studieprogram"        = "programnavn",
    "Utdanningstype"        = "utd_type",
    "Førstevalgs-\nsøkere\nper plass"        = "søker_pr_plass"

  ),
  title            = "Studier med høy konkurranse",
  size             = "extra compact",
  max_width        = 640,   # adjust to your iframe width
  col_shares = c(15,35,30,20),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "tabell_konkurranse",
  out_file         = "figurer/side_5_konkurranse_hc/tabeller/populære_min.html"
)





### POPULÆRE STUDIER - LANGVERSJON ### 
populære_max <- dt %>% 
  filter(aar == siste_aar & prioritet==1) %>% 
  group_by(inst_navn, programnavn,utd_type, studiekode) %>% 
  summarise(
    førstev = n_distinct(fnr),
    studieplasser = unique(studieplasser)
  ) %>%  
  mutate(
    søker_pr_plass = round((førstev/studieplasser), 1)
  ) %>% 
  arrange(desc(søker_pr_plass))

populære_max <- populære_max %>% 
  select("inst_navn","programnavn", "utd_type","førstev","studieplasser", "søker_pr_plass")


populære_max


make_hc_table_l(
  data = populære_max,
  display_cols = c(
    "Institusjon"       = "inst_navn",
    "Studieprogram"        = "programnavn",
    "Utdanningstype"        = "utd_type",
    "Førstevalgs-\nsøkere" = "førstev",
    "Studieplasser" = "studieplasser",
    "Førstevalgs-\nsøkere\nper plass"        = "søker_pr_plass"
    
  ),
  title            = "Studier med høy konkurranse",
  size             = "extra compact",
  max_width        = 1000,   # adjust to your iframe width
  col_shares = c(25,25,20,10,10,10),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "tabell_konkurranse",
  out_file         = "figurer/side_5_konkurranse_hc/tabeller/populære_max.html"
)



