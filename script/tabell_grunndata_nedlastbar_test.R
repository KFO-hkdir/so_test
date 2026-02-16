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

dt <- read_fst("data/so_uhg_hoved_master.fst")


# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)


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

################################## PROGRAMNIVÅ #################################

######################################
### TABELL - GRUNNTALL PROGRAMNIVÅ ###
######################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  filter(aar==siste_aar & status=="AKT") %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1 & status=="AKT") %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()


# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()


# Antall søkere med tilbud 
tilbud_aar <- dt %>% 
  filter(aar==siste_aar & tilbud=="J") %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(søkere_tilbud = n_distinct(regnr)) %>% 
  print()

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(aar==siste_aar & prioritet==1 & tilbud=="J") %>% 
  group_by(inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
  print()


# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%
  left_join(kvalifisert_aar, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type")) %>% 
  left_join(studplass_aar, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%
  left_join(tilbud_aar, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type")) %>% 
  left_join(tilbud_forste_aar, by = c("inst_navn","studiekode","programnavn", "studiested","utd_type")) 
dt_all


## Fjerner kolonner (for å teste med mindre tabell for bedre visning)
#dt_all$studiekode <- NULL
#dt_all$studiested <- NULL



### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_1 <- datatable(
  dt_all,
  filter = 'top',
  colnames = c(
    "Institusjon", "Studie- kode","Studienavn", "Studiested", "Utdanningstype",
    "Søkere", "Førstevalgs- søkere","Kvalifiserte søkere", "Studie- plasser", "Søkere m/tilbud", "Søkere m/tilbud førstevalg"
  ),
  extensions = 'Buttons',
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

tabell_1

saveWidget(tabell_1, "tabeller/grunndata_tabell_1_uhg.html", selfcontained = TRUE)





#############################################
### TABELL - GRUNNTALL PROGRAMNIVÅ PER ÅR ###
#############################################


### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1 & status=="AKT") %>% 
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()


# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()


# Antall søkere med tilbud 
tilbud_aar <- dt %>% 
  filter(tilbud=="J") %>% 
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(søkere_tilbud = n_distinct(regnr)) %>% 
  print()

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(prioritet==1 & tilbud=="J") %>% 
  group_by(aar, inst_navn, studiekode, programnavn, studiested, utd_type) %>% 
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
  print()



# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%
  left_join(kvalifisert_aar, by = c("aar","inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%  
  left_join(studplass_aar, by = c("aar","inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%
  left_join(tilbud_aar, by = c("aar","inst_navn","studiekode","programnavn", "studiested","utd_type")) %>%
  left_join(tilbud_forste_aar, by = c("aar","inst_navn","studiekode","programnavn", "studiested","utd_type")) 
dt_all


## Fjerner kolonner (vurdere å beholde, utvide visning i stedet)
#dt_all$studiekode <- NULL
#dt_all$studiested <- NULL


dt_pivoted <- dt_all %>%
  # Step 1: Long format — gather all metrics
  pivot_longer(
    cols = c(n_sokere, n_førstevalgssokere, kvalifiserte_søkere,antall_studieplasser, søkere_tilbud, søkere_tilbud_førstevalg),
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

dt_pivoted %>% 
  filter(studiekode==203566)

# Sjekker tilfeller av samme studiekode, flere programnavn (NB: Ikke filtrer på 2025 for dette!)
# dt_sjekk <- dt_pivoted %>% 
#   mutate(
#     studiekode = str_squish(str_to_upper(studiekode)),
#     programnavn = str_squish(str_to_title(programnavn)),
#     studiekode_programnavn = paste(studiekode, programnavn, sep = " - ")
#   )
# 
# dups_rows <- dt_sjekk %>% 
#   filter(variable == "Alle søkere") %>% 
#   group_by(studiekode_programnavn) %>% 
#   summarise(n=n()) %>% 
#   view()


## Fjerner kolonner (vurdere å beholde, utvide visning i stedet)
#dt_pivoted$studiekode <- NULL
#dt_pivoted$studiested <- NULL

dt_pivoted
#write_xlsx(dt_pivoted, "figurer_tabeller/grunndata_tabeller/nøkkeltall_programnivå_aar_med_NA.xlsx")
### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_2 <- datatable(
  dt_pivoted,
  filter = 'top',
  colnames = c(
    "Institusjon","Studie kode" ,"Studienavn", "Studiested", "Utdanningstype", "Variabel",
    "2015","2016","2017","2018","2019","2020", "2021", "2022", "2023", "2024","2025"
  ),
  extensions = 'Buttons',
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
    scrollX = TRUE
  ),
  width = "100%",
  rownames = FALSE
)

tabell_2 

saveWidget(tabell_2, "tabeller/grunndata_tabell_2_uhg.html", selfcontained = TRUE)





################################## INSTUTUSJONSNIVÅ ############################


####################################################
### TABELL - GRUNNTALL INSTITUSJONSNIVÅ SISTE ÅR ###
####################################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  filter(aar==siste_aar & status=="AKT") %>% 
  group_by(inst_navn) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1 & status=="AKT") %>% 
  group_by(inst_navn) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>% 
  group_by(inst_navn) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()


# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>% 
  group_by(inst_navn, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()


# Antall søkere med tilbud 
tilbud_aar <- dt %>% 
  filter(aar==siste_aar & tilbud=="J") %>% 
  group_by(inst_navn) %>% 
  summarise(søkere_tilbud = n_distinct(regnr)) %>% 
  print()

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(aar==siste_aar & prioritet==1 & tilbud=="J") %>% 
  group_by(inst_navn) %>% 
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
  print()



# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("inst_navn")) %>%
  left_join(kvalifisert_aar, by = c("inst_navn")) %>%  
  left_join(studplass_aar, by = c("inst_navn")) %>%
  left_join(tilbud_aar, by = c("inst_navn")) %>% 
  left_join(tilbud_forste_aar, by = c("inst_navn")) 

dt_all

### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_3 <- datatable(
  dt_all,
  filter = 'top',
  colnames = c(
    "Institusjon",
    "Søkere", "Førstevalgssøkere", "Kvalifiserte søkere", "Studieplasser", "Søkere m/tilbud", "Søkere m/tilbud - førstevalg"
  ),
  extensions = 'Buttons',
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

tabell_3

saveRDS(tabell_3, "figurer_tabeller/grunndata_tabeller/grunndata_tabell_3_uhg.rds")



################################################
### TABELL - GRUNNTALL INSTITUSJON  - PER ÅR ###
################################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, inst_navn) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1 & status=="AKT") %>% 
  group_by(aar, inst_navn) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, inst_navn) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print(n=130)


# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, inst_navn, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()


# Antall søkere med tilbud 
tilbud_aar <- dt %>% 
  filter(tilbud=="J") %>% 
  group_by(aar, inst_navn) %>% 
  summarise(søkere_tilbud = n_distinct(regnr)) %>% 
  print(n=130)

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(prioritet==1 & tilbud=="J") %>% 
  group_by(aar, inst_navn) %>% 
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
  print(n=130)

tilbud_forste_aar

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","inst_navn")) %>%
  left_join(kvalifisert_aar, by = c("aar","inst_navn")) %>%  
  left_join(studplass_aar, by = c("aar","inst_navn")) %>%
  left_join(tilbud_aar, by = c("aar","inst_navn")) %>% 
  left_join(tilbud_forste_aar, by = c("aar","inst_navn")) 
dt_all


# Pivoterer
dt_pivoted <- dt_all %>%
  # Step 1: Long format — gather all metrics
  pivot_longer(
    cols = c(n_sokere, n_førstevalgssokere,kvalifiserte_søkere, antall_studieplasser, søkere_tilbud, søkere_tilbud_førstevalg),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable,
                           "n_sokere" = "Alle søkere",
                           "n_førstevalgssokere" = "Førstevalgssøkere",
                           "kvalifiserte_søkere" = "Kvalifiserte søkere",                          
                           "antall_studieplasser" = "Studieplasser",
                           "kvalifiserte_søkere" = "Kvalifiserte søkere",
                           "søkere_tilbud" = "Søkere m/tilbud"
  )) %>%
  # Step 2: Wide format — spread years to columns
  pivot_wider(
    names_from = aar,
    values_from = value
  ) %>% 
  filter(!is.na(`2025`))

print(dt_pivoted, n = 100)

## Fjerner kolonner (for å teste med mindre tabell for bedre visning)
#NA


### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_4 <- datatable(
  dt_pivoted,
  filter = 'top',
  colnames = c(
    "Institusjon", "Variabel","2015","2016","2017","2018","2019", "2020", "2021", "2022", "2023", "2024","2025"
  ),
  extensions = 'Buttons',
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

tabell_4

saveRDS(tabell_4, "figurer_tabeller/grunndata_tabeller/grunndata_tabell_4_uhg.rds")



############################ UTDANNINGSOMRÅDE ############################

####################################################
### TABELL - GRUNNTALL UTDANNINGSOMRÅDE SISTE ÅR ###
####################################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  filter(aar==siste_aar & status=="AKT") %>% 
  group_by(utd_omr) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(aar==siste_aar & prioritet==1 & status=="AKT") %>% 
  group_by(utd_omr) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>% 
  group_by(utd_omr) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()


# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT" & aar==siste_aar) %>% 
  group_by(utd_omr, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()


# Antall søkere med tilbud 
tilbud_aar <- dt %>% 
  filter(aar==siste_aar & tilbud=="J") %>% 
  group_by(utd_omr) %>% 
  summarise(søkere_tilbud = n_distinct(regnr)) %>% 
  print()

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(aar==siste_aar & prioritet==1 & tilbud=="J") %>% 
  group_by(utd_omr) %>% 
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
  print()



# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("utd_omr")) %>%
  left_join(kvalifisert_aar, by = c("utd_omr")) %>%  
  left_join(studplass_aar, by = c("utd_omr")) %>%
  left_join(tilbud_aar, by = c("utd_omr")) %>%
  left_join(tilbud_forste_aar, by = c("utd_omr")) 
dt_all

### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_5 <- datatable(
  dt_all,
  filter = 'top',
  colnames = c(
    "Utdanningsområde",
    "Søkere", "Førstevalgssøkere","Kvalifiserte søkere", "Studieplasser", "Søkere m/tilbud", "Søkere m/tilbud - førstevalg"
  ),
  extensions = 'Buttons',
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

tabell_5

saveRDS(tabell_5, "figurer_tabeller/grunndata_tabeller/grunndata_tabell_5_uhg.rds")



#####################################################
### TABELL - GRUNNTALL UTDANNINGSOMRÅDE  - PER ÅR ###
#####################################################

### TABELL - GRUNNLAG ### 

sokere_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, utd_omr) %>% 
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1 & status=="AKT") %>% 
  group_by(aar, utd_omr) %>% 
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, utd_omr) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print(n=130)


# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, utd_omr, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()


# Antall søkere med tilbud 
tilbud_aar <- dt %>% 
  filter(tilbud=="J") %>% 
  group_by(aar, utd_omr) %>% 
  summarise(søkere_tilbud = n_distinct(regnr)) %>% 
  print(n=130)

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(prioritet==1 & tilbud=="J") %>% 
  group_by(aar, utd_omr) %>% 
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>% 
  print(n=130)

tilbud_forste_aar

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","utd_omr")) %>%
  left_join(kvalifisert_aar, by = c("aar","utd_omr")) %>% 
  left_join(studplass_aar, by = c("aar","utd_omr")) %>%
  left_join(tilbud_aar, by = c("aar","utd_omr")) %>% 
  left_join(tilbud_forste_aar, by = c("aar","utd_omr")) 

dt_all


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
                           "søkere_tilbud" = "Søkere m/tilbud - førstevalg"
  )) %>%
  # Step 2: Wide format — spread years to columns
  pivot_wider(
    names_from = aar,
    values_from = value
  ) %>% 
  filter(!is.na(`2025`))

print(dt_pivoted, n = 100)

## Fjerner kolonner (for å teste med mindre tabell for bedre visning)
#NA


### TABELL: GRUNNTALL - PROGRAMNIVÅ (søkere, førstevalg, studieplasser, kvalifiserte,tilbud) ###

tabell_6 <- datatable(
  dt_pivoted,
  filter = 'top',
  colnames = c(
    "Utdanningsområde", "Variabel","2015","2016","2017","2018","2019", "2020", "2021", "2022", "2023", "2024","2025"
  ),
  extensions = 'Buttons',
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

tabell_6

saveRDS(tabell_6, "figurer_tabeller/grunndata_tabeller/grunndata_tabell_6_uhg.rds")




############################ UTDANNINGSTYPE ############################

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

saveRDS(tabell_7, "figurer_tabeller/grunndata_tabeller/grunndata_tabell_7_uhg.rds")



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

saveRDS(tabell_8, "figurer_tabeller/grunndata_tabeller/grunndata_tabell_8_uhg.rds")



#### IKKE BRUKT! KUN TIL SJEKK #####



################################################
### TABELL - NØKKELTALL STUDIESTED  - PER ÅR ###
################################################

### TABELL - GRUNNLAG ###
sokere_aar <- dt %>%
  filter(status=="AKT") %>%
  group_by(aar, studiested) %>%
  summarise(n_sokere = n_distinct(regnr)) %>%
  print()

## Søkere per år

sokere_forste_aar <- dt %>%
  filter(prioritet==1 & status=="AKT") %>%
  group_by(aar, studiested) %>%
  summarise(n_førstevalgssokere = n_distinct(regnr)) %>%
  print()

# Antall (planlagte) studieplasser
studplass_aar <- dt %>%
  filter(status=="AKT") %>%
  group_by(aar, studiested) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>%
  print(n=130)


# Antall søkere kvalifisert til minst ett studium - RIKTIG ########################################################## **

kvalifisert_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, studiested, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()

# Antall søkere med tilbud
tilbud_aar <- dt %>%
  filter(tilbud=="J") %>%
  group_by(aar, studiested) %>%
  summarise(søkere_tilbud = n_distinct(regnr)) %>%
  print(n=130)

# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>%
  filter(prioritet==1 & tilbud=="J") %>%
  group_by(aar, studiested) %>%
  summarise(søkere_tilbud_førstevalg = n_distinct(regnr)) %>%
  print(n=130)

tilbud_forste_aar

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(sokere_forste_aar, by = c("aar","studiested")) %>%
  left_join(kvalifisert_aar, by = c("aar","studiested")) %>%  
  left_join(studplass_aar, by = c("aar","studiested")) %>%
  left_join(tilbud_aar, by = c("aar","studiested")) %>%
  left_join(tilbud_forste_aar, by = c("aar","studiested"))

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

write_xlsx(dt_pivoted, "analyser/alle/studiested_endring_nøkkeltall.xlsx")

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

saveRDS(tabell_8, "figurer_tabeller/grunndata_tabeller/grunndata_tabell_8_uhg.rds")