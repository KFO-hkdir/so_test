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


# Sjekker at alle utd_omr og utd_type har blitt kodet om
unique(dt$utd_omr)
unique(dt$utd_type)
