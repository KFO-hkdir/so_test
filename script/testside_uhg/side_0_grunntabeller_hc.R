################################################################
### GRUNNDATA - TABELL I SHINY MED MULIGHET FOR Å DRILLE NED ###
################################################################

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
library(DT)
library(scales)
library(htmlwidgets)


dt <- read_fst("data/so_sokertall_master_red.fst")

# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)


# CSS til tabellen 


dt_color_css <- tags$style(HTML("
/* Header color (like gt column labels) */
table.dataTable thead th {
  background-color: #F58220 !important;
  color: #ffffff !important;
}

/* Row striping color (like gt opt_row_striping) */
table.dataTable tbody tr:nth-child(even) {
  background-color: #fde9d9 !important;
}
table.dataTable tbody tr:nth-child(odd) {
  background-color: #ffffff !important;
}
"))


# Lager og lagrer folder 
folder_path <- "figurer/side_0_grunntabeller/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

# Fikser prgramnavn og studiested til siste programnavn og studiested - NB: Kun i 2025 - bruk andre variabler fra tableau i 2026

# Build the 2025 lookup (one row per studiekode)
map_2025 <- dt %>%
  filter(aar == 2025) %>%
  distinct(studiekode, programnavn, studiested)

# Join and overwrite for all rows where a 2025 mapping exists
dt_fixed <- dt %>%
  left_join(rename(map_2025,
                   programnavn_2025 = programnavn,
                   studiested_2025  = studiested),
            by = "studiekode") %>%
  mutate(
    programnavn = coalesce(programnavn_2025, programnavn),
    studiested  = coalesce(studiested_2025,  studiested)
  ) %>%
  select(-programnavn_2025, -studiested_2025)

dt_original <- dt
dt <- dt_fixed

dt$studiekode <- as.factor(dt$studiekode)

################################## PROGRAMNIVÅ #################################

######################################
### TABELL - GRUNNTALL PROGRAMNIVÅ ###
######################################

### TABELL - GRUNNLAG ### 

## Søkere per år

sokere_aar <- dt %>%
  filter(aar==siste_aar) %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Førstevalgssøkere per år

sokere_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1) %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(aar==siste_aar) %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()

# Førstevalgssøkere per planlagte studieplass?


# Andel kvinner blant førstevalgssøkere
andel_kvinner_forste <- dt %>%
  filter(aar == siste_aar & prioritet == 1) %>%
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>%
  summarise(andel_kvinner = mean(kjonn == "Kvinne", na.rm = TRUE) * 100) %>%
  mutate(andel_kvinner = paste0(scales::number(andel_kvinner, accuracy = 0.1, decimal.mark = ","), " %")) %>% 
  print()

andel_kvinner_forste
# Antall søkere kvalifisert til minst ett studium

# kvalifisert_aar <- dt %>%
#   filter(status=="AKT" & aar==siste_aar) %>% 
#   group_by(inst_navn, studiekode, programnavn, studiested, utd_type, regnr) %>%
#   summarise(is_qualified = any(kvalifisert == "J")) %>%
#   summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
#   print()


# Antall søkere med tilbud 
# tilbud_aar <- dt %>% 
#   filter(aar==siste_aar & tilbud=="J") %>% 
#   group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
#   summarise(søkere_tilbud = n_distinct(regnr)) %>% 
#   print()

# Antall søkere med tilbud (førstevalg)
# tilbud_forste_aar <- dt %>% 
#   filter(aar==siste_aar & prioritet==1 & tilbud=="J") %>% 
#   group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
#   summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
#   print()


# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%
  left_join(studplass_aar, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type")) %>% 
  left_join(andel_kvinner_forste, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type"))
dt_all

dt_all <- dt_all %>% 
  mutate(across(where(is.numeric), \(x) format(x, big.mark = " ", scientific = FALSE)))

source("funksjoner/tabell_funksjon_stor.R")

make_hc_table_l(
  data = dt_all,
  display_cols = c(
    "Institusjon"        = "inst_navn",
    "Studiekode"         = "studiekode",
    "Studienavn"         = "programnavn",
    "Studiested"         = "studiested",
    "Utdanningstype"     = "utd_type",
    "Studieplasser"      = "antall_studieplasser",
    "Søkere"             = "n_sokere",
    "Førstevalgs-\nsøkere"  = "n_førstevalgssokere",
    "Andel\nkvinnelige\nførstevalgs-\nsøkere" = "andel_kvinner"

  ),
  title            = "Nøkkeltall programnivå 2025",
  size             = "extra compact",
  max_width        = 1200,   # adjust to your iframe width
  col_shares = c(15 ,10,15,10,10,10, 10, 10,10),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "grunndata",
  out_file         = "figurer/side_0_grunntabeller_hc/tabeller/grunndata_tabell_1_uhg.html"
)





#############################################
### TABELL - GRUNNTALL PROGRAMNIVÅ PER ÅR ###
#############################################


### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1) %>% 
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()

# 
# # Antall søkere kvalifisert til minst ett studium
# 
# kvalifisert_aar <- dt %>%
#   filter(status=="AKT") %>% 
#   group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type, regnr) %>%
#   summarise(is_qualified = any(kvalifisert == "J")) %>%
#   summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
#   print()
# 
# 
# # Antall søkere med tilbud 
# tilbud_aar <- dt %>% 
#   filter(tilbud=="J") %>% 
#   group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
#   summarise(søkere_tilbud = n_distinct(regnr)) %>% 
#   print()
# 
# # Antall søkere med tilbud (førstevalg)
# tilbud_forste_aar <- dt %>% 
#   filter(prioritet==1 & tilbud=="J") %>% 
#   group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
#   summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
#   print()



# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%
  left_join(studplass_aar, by = c("aar","inst_navn","studiekode","programnavn", "studiested","utd_type")) 
dt_all


## Fjerner kolonner (vurdere å beholde, utvide visning i stedet)
#dt_all$studiekode <- NULL
#dt_all$studiested <- NULL


dt_pivoted <- dt_all %>%
  # Step 1: Long format — gather all metrics
  pivot_longer(
    cols = c(n_sokere, n_førstevalgssokere, ,antall_studieplasser),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable,
                           "n_sokere" = "Alle søkere",
                           "n_førstevalgssokere" = "Førstevalgssøkere",
                           "antall_studieplasser" = "Studieplasser"
  )) %>%
  # Step 2: Wide format — spread years to columns
  pivot_wider(
    names_from = aar,
    values_from = value
  ) %>% 
  filter(!is.na(`2025`))

print(dt_pivoted, n = 100)

dt_pivoted %>% 
  filter(studiekode==203566)

dt_pivoted

dt_pivoted <- dt_pivoted %>% 
  mutate(across(where(is.numeric), \(x) format(x, big.mark = " ", scientific = FALSE)))

source("funksjoner/tabell_funksjon_stor.R")


make_hc_table_l(
  data = dt_pivoted,
  display_cols = c(
    "Institusjon"       = "inst_navn",
    "Studie-\nkode"        = "studiekode",
    "Studienavn"        = "programnavn",
    "Studiested"        = "studiested",
    "Utdannings-\ntype"    = "utd_type",
    "Nøkkeltall"    = "variable",
    "2020" = "2020",
    "2021" = "2021",
    "2022" = "2022",
    "2023" = "2023",
    "2024" = "2024",
    "2025" = "2025"
  ),
  title            = "Nøkkeltall programnivå - tidsserier",
  size             = "extra compact",
  max_width        = 1400,   # adjust to your iframe width
  col_shares = c(18 ,5,15,7,15,10, 5, 5,5,5,5,5),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "grunndata",
  out_file         = "figurer/side_0_grunntabeller_hc/tabeller/grunndata_tabell_2_uhg.html"
)




################################## INSTUTUSJONSNIVÅ ############################


####################################################
### TABELL - GRUNNTALL INSTITUSJONSNIVÅ SISTE ÅR ###
####################################################

### TABELL - GRUNNLAG ### 

# Søkere per år 
sokere_aar <- dt %>%
  filter(aar==siste_aar) %>% 
  group_by(inst_navn) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Førstevalgssøkere per år
sokere_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1) %>% 
  group_by(inst_navn) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(aar==siste_aar) %>% 
  group_by(inst_navn) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()


# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("inst_navn")) %>%
  left_join(studplass_aar, by = c("inst_navn"))

dt_all

### TABELL ###

dt_all <- dt_all %>% 
  mutate(across(where(is.numeric), \(x) format(x, big.mark = " ", scientific = FALSE)))

source("funksjoner/tabell_funksjon_stor.R")

make_hc_table_l(
  data = dt_all,
  display_cols = c(
    "Institusjon"        = "inst_navn",

    "Søkere"             = "n_sokere",
    "Førstevalgs-\nsøkere"  = "n_førstevalgssokere",
    "Studieplasser"      = "antall_studieplasser"
  ),
  title            = "Nøkkeltall institusjonsnivå 2025",
  size             = "normal",
  max_width        = 1200,   # adjust to your iframe width
  col_shares = c(55, 15,15,15),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "grunndata",
  out_file         = "figurer/side_0_grunntabeller_hc/tabeller/grunndata_tabell_3_uhg.html"
)



################################################
### TABELL - GRUNNTALL INSTITUSJON  - PER ÅR ###
################################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  group_by(aar, inst_navn) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1) %>% 
  group_by(aar, inst_navn) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  group_by(aar, inst_navn) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print(n=130)

# 
# # Antall søkere kvalifisert til minst ett studium
# 
# kvalifisert_aar <- dt %>%
#   filter(status=="AKT") %>% 
#   group_by(aar, inst_navn, regnr) %>%
#   summarise(is_qualified = any(kvalifisert == "J")) %>%
#   summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
#   print()
# 
# 
# # Antall søkere med tilbud 
# tilbud_aar <- dt %>% 
#   filter(tilbud=="J") %>% 
#   group_by(aar, inst_navn) %>% 
#   summarise(søkere_tilbud = n_distinct(regnr)) %>% 
#   print(n=130)
# 
# # Antall søkere med tilbud (førstevalg)
# tilbud_forste_aar <- dt %>% 
#   filter(prioritet==1 & tilbud=="J") %>% 
#   group_by(aar, inst_navn) %>% 
#   summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
#   print(n=130)
# 
# tilbud_forste_aar

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","inst_navn")) %>%
  left_join(studplass_aar, by = c("aar","inst_navn")) 
dt_all


# Pivoterer
dt_pivoted <- dt_all %>%
  # Step 1: Long format — gather all metrics
  pivot_longer(
    cols = c(n_sokere, n_førstevalgssokere, antall_studieplasser),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable,
                           "n_sokere" = "Alle søkere",
                           "n_førstevalgssokere" = "Førstevalgssøkere",
                           "antall_studieplasser" = "Studieplasser"
  )) %>%
  # Step 2: Wide format — spread years to columns
  pivot_wider(
    names_from = aar,
    values_from = value
  ) %>% 
  filter(!is.na(`2025`))

print(dt_pivoted, n = 100)


### TABELL ###

dt_pivoted <- dt_pivoted %>% 
  mutate(across(where(is.numeric), \(x) format(x, big.mark = " ", scientific = FALSE)))

dt_pivoted

source("funksjoner/tabell_funksjon_stor.R")


make_hc_table_l(
  data = dt_pivoted,
  display_cols = c(
    "Institusjon"       = "inst_navn",
    "Nøkkeltall"    = "variable",
    "2018" = "2018",
    "2019" = "2019",
    "2020" = "2020",
    "2021" = "2021",
    "2022" = "2022",
    "2023" = "2023",
    "2024" = "2024",
    "2025" = "2025"
  ),
  title            = "Nøkkeltall institusjonsnivå - tidsserier",
  size             = "extra compact",
  max_width        = 1100,   # adjust to your iframe width
  col_shares = c(24,20,7,7,7,7,7,7,7,7),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "grunndata",
  out_file         = "figurer/side_0_grunntabeller_hc/tabeller/grunndata_tabell_4_uhg.html"
)


############################ UTDANNINGSOMRÅDE ############################

####################################################
### TABELL - GRUNNTALL UTDANNINGSOMRÅDE SISTE ÅR ###
####################################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  filter(aar==siste_aar) %>% 
  group_by(utd_omr) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1) %>% 
  group_by(utd_omr) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(aar==siste_aar) %>% 
  group_by(utd_omr) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()

# 
# # Antall søkere kvalifisert til minst ett studium
# 
# kvalifisert_aar <- dt %>%
#   filter(aar==siste_aar) %>% 
#   group_by(utd_omr, regnr) %>%
#   summarise(is_qualified = any(kvalifisert == "J")) %>%
#   summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
#   print()
# 
# 
# # Antall søkere med tilbud 
# tilbud_aar <- dt %>% 
#   filter(aar==siste_aar & tilbud=="J") %>% 
#   group_by(utd_omr) %>% 
#   summarise(søkere_tilbud = n_distinct(regnr)) %>% 
#   print()
# 
# # Antall søkere med tilbud (førstevalg)
# tilbud_forste_aar <- dt %>% 
#   filter(aar==siste_aar & prioritet==1 & tilbud=="J") %>% 
#   group_by(utd_omr) %>% 
#   summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
#   print()



# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("utd_omr")) %>%
  left_join(studplass_aar, by = c("utd_omr")) 
dt_all

### TABELL ###

dt_all <- dt_all %>% 
  mutate(across(where(is.numeric), \(x) format(x, big.mark = " ", scientific = FALSE)))

source("funksjoner/tabell_funksjon_stor.R")

make_hc_table_l(
  data = dt_all,
  display_cols = c(
    "Utdanningsområde"        = "utd_omr",
    
    "Søkere"             = "n_sokere",
    "Førstevalgs-\nsøkere"  = "n_førstevalgssokere",
    "Studieplasser"      = "antall_studieplasser"
  ),
  title            = "Nøkkeltall utdanningsområdenivå 2025",
  size             = "normal",
  max_width        = 1200,   # adjust to your iframe width
  scroll_height = "490px",
  col_shares = c(55, 15,15,15),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "grunndata",
  out_file         = "figurer/side_0_grunntabeller_hc/tabeller/grunndata_tabell_5_uhg.html"
)




#####################################################
### TABELL - GRUNNTALL UTDANNINGSOMRÅDE  - PER ÅR ###
#####################################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  group_by(aar, utd_omr) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1) %>% 
  group_by(aar, utd_omr) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  group_by(aar, utd_omr) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print(n=130)

# 
# # Antall søkere kvalifisert til minst ett studium
# 
# kvalifisert_aar <- dt %>%
#   filter(status=="AKT") %>% 
#   group_by(aar, utd_omr, regnr) %>%
#   summarise(is_qualified = any(kvalifisert == "J")) %>%
#   summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
#   print()
# 
# 
# # Antall søkere med tilbud 
# tilbud_aar <- dt %>% 
#   filter(tilbud=="J") %>% 
#   group_by(aar, utd_omr) %>% 
#   summarise(søkere_tilbud = n_distinct(regnr)) %>% 
#   print(n=130)
# 
# # Antall søkere med tilbud (førstevalg)
# tilbud_forste_aar <- dt %>% 
#   filter(prioritet==1 & tilbud=="J") %>% 
#   group_by(aar, utd_omr) %>% 
#   summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
#   print(n=130)
# 
# tilbud_forste_aar

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","utd_omr")) %>%
  left_join(studplass_aar, by = c("aar","utd_omr")) 

dt_all


# Pivoterer
dt_pivoted <- dt_all %>%
  # Step 1: Long format — gather all metrics
  pivot_longer(
    cols = c(n_sokere, n_førstevalgssokere, antall_studieplasser),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable,
                           "n_sokere" = "Alle søkere",
                           "n_førstevalgssokere" = "Førstevalgssøkere",
                           "antall_studieplasser" = "Studieplasser"
  )) %>%
  # Step 2: Wide format — spread years to columns
  pivot_wider(
    names_from = aar,
    values_from = value
  ) %>% 
  filter(!is.na(`2025`))

print(dt_pivoted, n = 100)



### TABELL ###
dt_pivoted <- dt_pivoted %>% 
  mutate(across(where(is.numeric), \(x) format(x, big.mark = " ", scientific = FALSE)))

dt_pivoted

source("funksjoner/tabell_funksjon_stor.R")


make_hc_table_l(
  data = dt_pivoted,
  display_cols = c(
    "Institusjon"       = "utd_omr",
    "Nøkkeltall"    = "variable",
    "2018" = "2018",
    "2019" = "2019",
    "2020" = "2020",
    "2021" = "2021",
    "2022" = "2022",
    "2023" = "2023",
    "2024" = "2024",
    "2025" = "2025"
  ),
  title            = "Nøkkeltall utdanningsområdenivå - tidsserier",
  size             = "extra compact",
  max_width        = 1100,   # adjust to your iframe width
  col_shares = c(24,20,7,7,7,7,7,7,7,7),
  filter_cols      = TRUE,
  export_xlsx      = TRUE,
  export_file_stem = "grunndata",
  out_file         = "figurer/side_0_grunntabeller_hc/tabeller/grunndata_tabell_6_uhg.html"
)










######################### UTDANNINGSTYPE - IKKE BRUKT! #########################

####################################################
### TABELL - GRUNNTALL UTDANNINGSTYPE SISTE ÅR ###
####################################################

## TABELL - GRUNNLAG ###

sokere_aar <- dt %>%
  filter(aar==siste_aar  & status=="AKT") %>%
  group_by(utd_type) %>%
  summarise(n_sokere = n_distinct(regnr)) %>%
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1 & status=="AKT") %>%
  group_by(utd_type) %>%
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>%
  print()

# Antall (planlagte) studieplasser
studplass_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>%
  group_by(utd_type) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>%
  print()


# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>% 
  group_by(utd_type, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()


# Antall søkere med tilbud
tilbud_aar <- dt %>%
  filter(aar==siste_aar & tilbud=="J") %>%
  group_by(utd_type) %>%
  summarise(søkere_tilbud = n_distinct(regnr)) %>%
  print()

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1 & tilbud=="J") %>%
  group_by(utd_type) %>%
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>%
  print()



# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("utd_type")) %>%
  left_join(kvalifisert_aar, by = c("utd_type")) %>%  
  left_join(studplass_aar, by = c("utd_type")) %>%
  left_join(tilbud_aar, by = c("utd_type")) %>%
  left_join(tilbud_forste_aar, by = c("utd_type"))
dt_all

write_xlsx(dt_all, "analyser/alle/utdtype_2025_nøkkeltall.xlsx")

### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_7 <- datatable(
  dt_all,
  filter = 'top',
  colnames = c(
    "Utdanningstype",
    "Søkere", "Førstevalgssøkere", "Kvalifiserte søkere", "Studieplasser", "Søkere m/tilbud", "Søkere m/tilbud - førstevalg"
  ),
  extensions = 'Buttons',
  class = "table",  # ensure bootstrap class is present (or remove it)
  options = list(
    dom = 'Blfrtip',
    buttons = list(
      list(extend = 'copy', text = 'Kopier', exportOptions = list(modifier = list(page = "all"))),
      list(extend = 'csv', text = 'Last ned CSV', filename = 'grunnlagsdata_so_2025', exportOptions = list(modifier = list(page = "all"))),
      list(extend = 'excel', text = 'Last ned Excel', filename = 'grunnlagsdata_so_2025', exportOptions = list(modifier = list(page = "all")))
    ),
    language = list(
      lengthMenu = "Vis _MENU_ rader"
    ),
    pageLength = 25,
    paging = FALSE,
    scrollY = "600px",
    autoWidth = TRUE,
    scrollX = FALSE
  ),
  width = "100%",
  rownames = FALSE
)

tabell_7

tabell_7 <- htmlwidgets::prependContent(tabell_7, dt_color_css) # Legger til CSS

tabell_7


saveWidget(tabell_7, file.path(folder_path, "grunndata_tabell_7_uhg.html"), selfcontained = TRUE)





#####################################################
### TABELL - GRUNNTALL UTDANNINGSTYPE  - PER ÅR ###
#####################################################

### TABELL - GRUNNLAG ###

sokere_aar <- dt %>%
  filter(status=="AKT") %>%
  group_by(aar, utd_type) %>%
  summarise(n_sokere = n_distinct(regnr)) %>%
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1 & status=="AKT") %>%
  group_by(aar, utd_type) %>%
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>%
  print()

# Antall (planlagte) studieplasser
studplass_aar <- dt %>%
  filter(status=="AKT") %>%
  group_by(aar, utd_type) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>%
  print(n=130)


# Antall søkere kvalifisert til minst ett studium - RIKTIG ########################################################## **

kvalifisert_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, utd_type, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()

# Antall søkere med tilbud
tilbud_aar <- dt %>%
  filter(tilbud=="J") %>%
  group_by(aar, utd_type) %>%
  summarise(søkere_tilbud = n_distinct(regnr)) %>%
  print(n=130)

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>%
  filter(prioritet==1 & tilbud=="J") %>%
  group_by(aar, utd_type) %>%
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>%
  print(n=130)

tilbud_forste_aar

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","utd_type")) %>%
  left_join(kvalifisert_aar, by = c("aar","utd_type")) %>%  
  left_join(studplass_aar, by = c("aar","utd_type")) %>%
  left_join(tilbud_aar, by = c("aar","utd_type")) %>%
  left_join(tilbud_forste_aar, by = c("aar","utd_type"))

dt_all
view(dt_all)

# Pivoterer
dt_pivoted <- dt_all %>%
  # Step 1: Long format — gather all metrics
  pivot_longer(
    cols = c(n_sokere, n_førstevalgssokere, kvalifiserte_søkere, antall_studieplasser, søkere_tilbud, søkere_tilbud_førstevalg),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable,
                           "n_sokere" = "Alle søkere",
                           "n_førstevalgssokere" = "Førstevalgssøkere",
                           "kvalifiserte_søkere" = "Kvalifiserte søkere",                          
                           "antall_studieplasser" = "Studieplasser",
                           "søkere_tilbud" = "Søkere m/tilbud",
                           "søkere_tilbud_førstevalg" = "Søkere m/tilbud - førstevalg"
  )) %>%
  # Step 2: Wide format — spread years to columns
  pivot_wider(
    names_from = aar,
    values_from = value
  ) %>%
  filter(!is.na(`2025`))

print(dt_pivoted, n = 100)
view(dt_pivoted)

write_xlsx(dt_pivoted, "analyser/alle/utdtype_endring_nøkkeltall.xlsx")

## Fjerner kolonner (for å teste med mindre tabell for bedre visning)
#NA


### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_8 <- datatable(
  dt_pivoted,
  filter = 'top',
  colnames = c(
    "Utdanningstype", "Variabel", "2015","2016","2017","2018","2019", "2020", "2021", "2022", "2023", "2024","2025"
  ),
  extensions = 'Buttons',
  class = "table",  # ensure bootstrap class is present (or remove it)
  options = list(
    dom = 'Blfrtip',
    buttons = list(
      list(extend = 'copy', text = 'Kopier', exportOptions = list(modifier = list(page = "all"))),
      list(extend = 'csv', text = 'Last ned CSV', filename = 'grunnlagsdata_so_2025', exportOptions = list(modifier = list(page = "all"))),
      list(extend = 'excel', text = 'Last ned Excel', filename = 'grunnlagsdata_so_2025', exportOptions = list(modifier = list(page = "all")))
    ),
    language = list(
      lengthMenu = "Vis _MENU_ rader"
    ),
    pageLength = 25,
    paging = FALSE,
    scrollY = "600px",
    autoWidth = TRUE,
    scrollX = FALSE
  ),
  width = "100%",
  rownames = FALSE
)

tabell_8

tabell_8 <- htmlwidgets::prependContent(tabell_8, dt_color_css) # Legger til CSS

tabell_8


saveWidget(tabell_8, file.path(folder_path, "grunndata_tabell_8_uhg.html"), selfcontained = TRUE)



