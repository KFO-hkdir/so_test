
## Sørger for at norske bokstaver (æ,ø,å) kan tolkes av R
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

## Nyttige pakker
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



dt <- read_csv2("data/SO_poenggrense_grunnfil_UHG_25_ny.csv")

### Nye variabelnavn
names(dt)
names(dt) <- c("kvote","inst_navn",
               "inst_navn2","opptaksrunde",
               "poenggrense","poenggrense2",
               "sesong","studiekode", "studiepoeng",
               "studiested","programnavn",
               "tid","utd_type",
               "aar","venteliste")

print(dt, n=20)

# Fjerner mellomrom og gjør studiekode numerisk
dt <- dt %>% 
  mutate(
    studiekode = str_remove_all(studiekode, " "),   # a) remove spaces
    studiekode = as.numeric(studiekode)             # b) convert to numeric
  )


# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")


#### Lager nye verdier for utdanningsområde og -type
dt <- dt %>%
  separate(utd_type, into = c("utd_omr", "subkategori"), sep = " - ", remove = FALSE) %>%
  mutate(utd_omr = trimws(utd_omr))
unique(dt$utd_omr)

dt <- dt %>%
  mutate(
    utd_omr = recode(utd_omr,
                     "INFOTEKN" = "Informasjonsteknologi",
                     "LANDOGHAVBRUK" = "Land- og havbruk",
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

unique(dt$utd_omr)
unique(dt$utd_type)
# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)


### Sjekker variabler 

unique(dt$opptaksrunde) # Bare hovedopptak
unique(dt$sesong) # Bare hovedopptak
unique(dt$kvote) # ORD og ORDF
unique(dt$aar)
unique(dt$studiested)


##############
### TABELL ###
##############


### Tilrettelegger data 

# ORDF
pgrense_ordf <- dt %>% 
  filter(aar == siste_aar & kvote=="ORDF" & !poenggrense %in% c(-1, -2)) %>% 
  group_by(inst_navn, programnavn,studiekode, utd_type, poenggrense, venteliste) %>% 
  arrange(desc(poenggrense))

pgrense_ordf

pgrense_ordf_min <- pgrense_ordf %>% 
  select("inst_navn","programnavn", "studiekode","utd_type","poenggrense","venteliste")

pgrense_ordf_min


# ORD 
pgrense_ord <- dt %>% 
  filter(aar == siste_aar & kvote=="ORD" & !poenggrense %in% c(-1, -2)) %>% 
  group_by(inst_navn, programnavn,studiekode, utd_type, poenggrense, venteliste) %>% 
  arrange(desc(poenggrense))

pgrense_ord

pgrense_ord_min <- pgrense_ord %>% 
  select("inst_navn","programnavn", "studiekode","utd_type","poenggrense","venteliste")

pgrense_ord_min

## Formatterer og setter opp tabell med DT - ORDF

pgrense_ordf_tabell <- datatable(
  pgrense_ordf_min,
  rownames = FALSE,
  options = list(
    pageLength = 10,       # Show 10 rows per page
    searchHighlight = TRUE,
    language = list(search = "Søk:", lengthMenu = "Vis _MENU_ rader")
  ),
  colnames = c("Institusjon", "Studieprogram", "Studiekode", "Utdanningstype", "Poenggrense", "Venteliste")
)

pgrense_ordf_tabell
saveWidget(pgrense_ordf_tabell, "../tabeller/pgrense_ordf_tabell.html", selfcontained = TRUE)

## Formatterer og setter opp tabell med DT - ORD

pgrense_ord_tabell <- datatable(
  pgrense_ord_min,
  rownames = FALSE,
  options = list(
    pageLength = 10,       # Show 10 rows per page
    searchHighlight = TRUE,
    language = list(search = "Søk:", lengthMenu = "Vis _MENU_ rader")
  ),
  colnames = c("Institusjon", "Studieprogram", "Studiekode", "Utdanningstype", "Poenggrense", "Venteliste")
)

pgrense_ord_tabell
saveWidget(pgrense_ord_tabell, "../tabeller/pgrense_ord_tabell.html", selfcontained = TRUE)
