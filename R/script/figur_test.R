

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

dt <- read_fst("data/so_uhg_hoved_master.fst")


# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)


#### Om kvalifisert og tilbud
# Søker er kvalifisert til ett eller flere av søknadsalternativene
# Søker mottar tilbud på KUN det høyest rangerte studiet de er kvalifisert til

# Dermed: gir kun mening å snakke om kvalifiserte søkere til a) spesifikke rangeringer
# eller b) studieprogrammer. Vi kan også snakke om søkere kvalifisert til minst ett 
# studieprogram.

# For tilbud kan vi snakke om søkere med (minst ett) tilbud, og f.eks. søkere med tilbud
# på førstevalg.

# Kan sjekke hvor tilbud er gitt (siden det er maks kun ett tilbud per søker)

list.files("figurer_tabeller/2_utd_omr")
list.files("figurer_tabeller/2_utd_type")



####################
### LINJEDIAGRAM ###
####################

### Søkere/førstevalgssøkere og planlagte studieplasser - nasjonalt ###

## Søkere per år

sokere_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar) %>%
  summarise(n_sokere = n_distinct(regnr)) %>% 
  print()

# Antall (planlagte) studieplasser 
studplass_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm=TRUE)) %>% 
  print()

# Antall studier (minus trukne studier)
studier_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar) %>%
  summarise(antall_studier = n_distinct(studiekode)) %>% 
  print()

# Antall trukne studier 
studier_aar <- dt %>%
  filter(status=="TRU") %>% 
  group_by(aar) %>%
  summarise(antall_studier = n_distinct(studiekode)) %>% 
  print()

# Antall søkere kvalifisert til minst ett studium

kvalifisert_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, regnr) %>%
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalifiserte_søkere = sum(is_qualified)) %>% 
  print()

# Antall kvalifiserte førstevalgssøkere
kvalifisert_forste_aar <- dt %>% 
  filter(prioritet==1 & kvalifisert=="J") %>% 
  group_by(aar) %>% 
  summarise(kvalifiserte_førstesøkere = n_distinct(regnr)) %>% 
  print()


# Antall søkere med tilbud (på ett alternativ)
tilbud_aar <- dt %>% 
  filter(tilbud=="J") %>% 
  group_by(aar) %>% 
  summarise(tilbud = n_distinct(regnr)) %>% 
  print()


# Antall søkere med tilbud (førstevalg)
tilbud_forste_aar <- dt %>% 
  filter(prioritet==1 & tilbud=="J") %>% 
  group_by(aar) %>% 
  summarise(tilbud_førstevalg = n_distinct(regnr)) %>% 
  print()

# Antall kvalifiserte søkere uten tilbud
kvalifisert_ikketilbud_aar <- dt %>%
  filter(status=="AKT") %>% 
  group_by(aar, regnr) %>%
  filter(all(tilbud=="N")) %>% 
  summarise(is_qualified = any(kvalifisert == "J")) %>%
  summarise(kvalif_søkere_ikktilbud = sum(is_qualified)) %>% 
  print()

# 🔄 Merge all the summaries
dt_all <- sokere_aar %>%
  left_join(kvalifisert_aar, by = "aar") %>% 
  left_join(studplass_aar, by = "aar") %>%
  left_join(tilbud_aar, by = "aar") %>% 
  left_join(tilbud_forste_aar, by = "aar") 
dt_all

# 🔄 Reshape to long format for plotting
dt_rs <- dt_all %>%
  pivot_longer(cols = -aar,
               names_to = "variable",
               values_to = "value")

dt_rs %>% 
  print(n=30)

# Endrer på rekkefølge + navn til labels 
dt_rs <- dt_rs %>%
  mutate(variable = factor(variable,
                           levels = c("n_sokere", 
                                      "kvalifiserte_søkere",
                                      "antall_studieplasser", 
                                      "tilbud",
                                      "tilbud_førstevalg"),
                           labels = c("Søkere", 
                                      "Kvalifiserte søkere",
                                      "Studieplasser", 
                                      "Søkere med tilbud",
                                      "Søkere med tilbud (førstevalg)")))

dt_rs %>% 
  print(n=30)


# Setter opp ggplot-figur 

fig_nasj_uhg <- ggplot(dt_rs, aes(x = aar, y = value, color = variable)) + 
  geom_line(size = 2) +
  geom_point(size = 5) +
  geom_text(aes(label = value),
            nudge_x = 0,
            vjust = -1.5,
            check_overlap = TRUE,
            show.legend = FALSE,
            size = 3,
            fontface = "bold",
            color = "black") +
  scale_color_manual(values = hkdir_farger[1:6]) +
  labs(
    title = "Søkere og antall studieplasser",
    x = "",
    y = "",
    color = ""
  ) +
  scale_y_continuous(limits = c(0, 170000)) +
  scale_x_continuous(breaks = unique(dt_rs$aar), labels = as.integer(unique(dt_rs$aar))) +
  theme_minimal(base_size = 14) +  # Clean theme with readable font size
  theme(
    legend.position = "bottom", 
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    axis.title.x = element_text( size = 12, face = "bold"),
    axis.title.y = element_text( size = 12, face = "bold")
  )


fig_nasj_uhg
fig_nasj_uhg_widget <- plotly::ggplotly(fig_nasj_uhg)

saveWidget(fig_nasj_uhg_widget, "../fig_nasj_uhg.html", selfcontained = TRUE)


## Forenklet ggplot 
fig_nasj_uhg_alt <- ggplot(dt_rs, aes(x = aar, y = value, color = variable)) +
  geom_line(size = 2) +
  geom_point(
    size = 5,
    aes(text = paste0(variable, ": ", scales::comma(value)))   # tooltip here
  ) +
  scale_color_manual(values = hkdir_farger[1:6]) +
  labs(x = "", y = "", color = "") +
  scale_y_continuous(limits = c(0, 170000)) +
  scale_x_continuous(
    breaks = unique(dt_rs$aar),
    labels = as.integer(unique(dt_rs$aar))
  ) +
  theme_minimal(base_size = 14)

fig_nasj_uhg_alt_widget <- plotly::ggplotly(fig_nasj_uhg_alt, tooltip = "text")
fig_nasj_uhg_alt_widget


fig_nasj_uhg_alt_widget <- fig_nasj_uhg_alt_widget %>%
  layout(
    legend = list(
      orientation = "h",       # horizontal
      x = 0.5,                 # center horizontally
      xanchor = "center",
      y = -0.2                 # push it below the plot
    )
  )

fig_nasj_uhg_alt_widget %>% 
  layout(hovermode = "closest")
saveWidget(fig_nasj_uhg_alt_widget, "../fig_nasj_uhg_alt.html", selfcontained = TRUE)

## Direkte i plotly


fig <- plot_ly(
  data = dt_rs,
  x = ~aar,
  y = ~value,
  color = ~variable,
  type = "scatter",
  mode = "lines+markers"
)

fig


dt_rs$tooltip <- paste0(
  dt_rs$variable, ": ", comma(dt_rs$value)
)


fig <- plot_ly(
  data = dt_rs,
  x = ~aar,
  y = ~value,
  color = ~variable,
  colors = hkdir_farger,    # your custom color vector
  text = ~tooltip,
  hoverinfo = "text",
  type = "scatter",
  mode = "lines+markers"
)

fig
