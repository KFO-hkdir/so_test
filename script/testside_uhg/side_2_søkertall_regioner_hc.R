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

library(highcharter)
library(jsonlite)

library(glue)

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

##################################### KART #####################################

##############################
### NORGESKART MED TOOLTIP ###
##############################

### SO-DATA - TIL TOOLTIP (siste_aar) ###

# Søkere fra:
sokere_fra <- dt %>% 
  filter(aar == siste_aar) %>% 
  group_by(fylke_soker) %>% 
  summarise(n_sokere_fra = n_distinct(fnr)) %>% 
  bind_rows(
    summarise(., fylke_soker = "Total", n_sokere_fra = sum(n_sokere_fra))
  )

# Søkere til (førstevalg):
sokere_til <- dt %>% 
  filter(aar == siste_aar & prioritet == 1) %>% 
  group_by(studiested_fylke) %>% 
  summarise(n_sokere_til = n_distinct(fnr)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_sokere_til = sum(n_sokere_til))
  )

# Antall institusjoner:
n_inst <- dt %>% 
  filter(aar == siste_aar) %>% 
  group_by(studiested_fylke) %>% 
  summarise(n_inst = n_distinct(inst_nr)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_inst = sum(n_inst))
  )

# Antall studier:
n_stud <- dt %>% 
  filter(aar == siste_aar) %>% 
  group_by(studiested_fylke) %>% 
  summarise(n_stud = n_distinct(studiekode)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_stud = sum(n_stud))
  )

# Planlagte studieplasser:
n_studplass <- dt %>% 
  filter(aar == siste_aar) %>% 
  group_by(studiested_fylke) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(n_studplass = sum(studieplasser, na.rm = TRUE)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_studplass = sum(n_studplass))
  ) %>% 
  print()

# Antall planlagte studieplasser (overall count):
total_studieplasser <- dt %>% 
  group_by(aar) %>%
  distinct(studiekode, studieplasser) %>%
  summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE)) %>% 
  print()

# Merge all "inst_fylke" based summaries
dt_tooltip <- sokere_til %>%
  left_join(n_inst, by = "studiested_fylke") %>%
  left_join(n_stud, by = "studiested_fylke") %>%
  left_join(n_studplass, by = "studiested_fylke") %>% 
  print()

# Add "Søkere fra" (only matching fylke with inst_fylke)
dt_tooltip <- dt_tooltip %>%
  left_join(sokere_fra, by = c("studiested_fylke" = "fylke_soker"))

dt_tooltip <- dt_tooltip %>%
  mutate(soker_pr_plass = round(n_sokere_til/n_studplass,1))


# Display final data
print(dt_tooltip)

##################################
## LAGER KARTET MED HIGHCHARTS ###
##################################

library(highcharter)
library(dplyr)



# Check highchart keys for counties
nor_keys <- get_data_from_map(download_map_data("countries/no/no-all"))
print(nor_keys[, c("hc-key", "name")])

# Prepare your data (same dt_tooltip you already have)
map_data <- dt_tooltip %>%
  filter(studiested_fylke != "Total") %>%
  mutate(
    # Highcharts Norway map uses specific region codes
    # You'll need to match your fylke names to Highcharts' codes (e.g. "no-os" for Oslo)
    hc_key = case_when(
      studiested_fylke == "Oslo"            ~ "no-os",
      studiested_fylke == "Akershus"        ~ "no-ak",
      studiested_fylke == "Vestland"        ~ "no-vl",
      studiested_fylke == "Trøndelag"       ~ "no-td",
      studiested_fylke == "Rogaland"        ~ "no-ro",
      studiested_fylke == "Innlandet"       ~ "no-in",
      studiested_fylke == "Nordland"        ~ "no-no",
      studiested_fylke == "Agder"           ~ "no-ag",
      studiested_fylke == "Møre og Romsdal" ~ "no-mr",
      studiested_fylke == "Troms"           ~ "no-tr", # check Highcharts keys
      studiested_fylke == "Finnmark"        ~ "no-fi",
      studiested_fylke == "Vestfold"        ~ "no-vf",
      studiested_fylke == "Telemark"        ~ "no-te",
      studiested_fylke == "Buskerud"        ~ "no-bu",
      studiested_fylke == "Østfold"         ~ "no-of",
      TRUE ~ NA_character_
    )
  )

kart_soking_uhg <- hcmap(
  map = "countries/no/no-all",
  data = map_data,
  value = "n_sokere_til",
  joinBy = c("hc-key", "hc_key"),
  name = "Søkere (førstevalg)",
  dataLabels = list(enabled = FALSE, format = "{point.name}"), # Fylkesnavn i labels
  tooltip = list(
    headerFormat = "",
    pointFormat = paste0(
      "<b>{point.name}</b><br>",
      "Søkere til (førstevalg): {point.value}<br>",
      "Søkere fra: {point.n_sokere_fra}<br>",
      "Institusjoner: {point.n_inst}<br>",
      "Studier: {point.n_stud}<br>",
      "Studieplasser: {point.n_studplass}<br>",
      "Søkere per plass: {point.soker_pr_plass}"
    )
  )
) %>%
  hc_colorAxis(
    minColor = "#FFF3E0",
    maxColor = "#E72F72"
  ) %>%
  hc_chart(
    height = 850
  ) %>%
  # Inject mapView projection as raw option
  hc_add_series_list(list()) %>%
  hc_title(text = "Søking til høyere utdanning") %>%
  hc_mapNavigation(enabled = TRUE) %>% 
  hc_exporting(enabled = TRUE) # Export button

# Inject mapView directly into the highchart object's params
kart_soking_uhg$x$hc_opts$mapView <- list(
  projection = list(name = "LambertConformalConic") # Kan også teste "Miller", "EqualEarth" eller "LambertConformalConic" her
)


# Change the map colour and remove legend
kart_soking_uhg$x$hc_opts$colorAxis <- list(
  minColor = hkdir_farger[3],
  maxColor = hkdir_farger[3],
  visible = FALSE
)

kart_soking_uhg$x$hc_opts$series[[1]]$color <- hkdir_farger[3]

kart_soking_uhg$x$hc_opts$chart$backgroundColor <- "#F0F0F0"  # ← light gray background


kart_soking_uhg

# Lagrer

folder_path <- "figurer/side_2_geografi_hc/kart/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

htmlwidgets::saveWidget(
  widget = kart_soking_uhg,
  file = file.path(folder_path, "kart_soking_uhg.html"),
  selfcontained = TRUE
)

##########################################
### SANKEY DIAGRAM - MED GOOGLE CHARTS ###
##########################################

########################### SANKEY-DIAGRAM #####################################

###############################################################
### SANKEY DIAGRAM SØKERE I OG UTENFOR FYLKE - TIL LANDSDEL ###
###############################################################

#############################################
### Forbereder datagrunnlaget til figuren ###
#############################################


## Forbereder dataene

fylker_tilfra <- dt %>%
  filter(aar==siste_aar & prioritet==1 & !fylke_soker =="UKJENT") %>% 
  group_by(fylke_soker, studiested_fylke) %>%
  summarise(n_applicants = n_distinct(regnr), .groups = "drop")

fylker_tilfra


#Legge til filtrering på søking til hjemstedsfylke, eller ut av fylket
fylker_tilfra <- fylker_tilfra %>%
  mutate(hjem_ut = case_when(
    fylke_soker == studiested_fylke ~ "Førstevalg i hjemfylket",
    fylke_soker != studiested_fylke ~ "Førstevalg utenfor hjemfylket"
  ))

# Sjekker koding
fylker_tilfra

# Legge til kolonne med studiested_landsdel
fylker_tilfra <- fylker_tilfra %>%
  mutate(
    studiested_landsdel = case_when(
      studiested_fylke %in% c("Troms", "Finnmark", "Nordland") ~ "Nord-Norge",
      studiested_fylke %in% c("Trøndelag") ~ "Trøndelag",
      studiested_fylke %in% c("Møre og Romsdal", "Vestland", "Rogaland") ~ "Vestlandet",
      studiested_fylke %in% c("Østfold", "Akershus", "Buskerud", "Telemark",
                              "Innlandet", "Vestfold", "Oslo") ~ "Østlandet",
      studiested_fylke %in% c("Agder") ~ "Sørlandet",
      TRUE ~ NA_character_   # fallback if an unexpected value appears
    )
  )

# Sjekker koding
fylker_tilfra %>% 
  print(n=160)


######################################################
### SANKEY MED GOOGLE CHARTS - MED TALL OG ANDELER ###
######################################################


# --- 2. Aggregate data for the Sankey diagram -----------------------------

sankey_data <- fylker_tilfra %>%
  group_by(hjem_ut, studiested_landsdel) %>%
  summarise(value = sum(n_applicants, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(studiested_landsdel))  # remove rows without a matching region

# --- 3. Add totals and percentages for both sides -------------------------

# Right-side totals (study regions)
right_labels <- sankey_data %>%
  group_by(studiested_landsdel) %>%
  summarise(total_right = sum(value), .groups = "drop") %>%
  mutate(share_right = total_right / sum(total_right) * 100)

# Left-side totals (home vs outside)
left_labels <- sankey_data %>%
  group_by(hjem_ut) %>%
  summarise(total_left = sum(value), .groups = "drop") %>%
  mutate(share_left = total_left / sum(total_left) * 100)

# Merge both sets of info
sankey_data <- sankey_data %>%
  left_join(right_labels, by = "studiested_landsdel") %>%
  left_join(left_labels, by = "hjem_ut") %>%
  mutate(
    hjem_ut_label = paste0(
      hjem_ut,
      " (", total_left, ", ", round(share_left, 1), "%)"
    ),
    studiested_landsdel_label = paste0(
      studiested_landsdel,
      " (", total_right, ", ", round(share_right, 1), "%)"
    )
  )

# --- 4. Convert to JavaScript row format ---------------------------------

sankey_rows <- sankey_data %>%
  mutate(js_row = paste0("['", hjem_ut_label, "', '", studiested_landsdel_label, "', ", value, "]")) %>%
  pull(js_row) %>%
  paste(collapse = ",\n          ")

# --- 5. Define HTML template ---------------------------------------------

html_template <- glue('
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
    <style>
      html, body {{ margin:0; padding:0; }}
      #wrap {{ max-width:640px; margin:0 auto; font-family:sans-serif; }}
      #sankey_chart {{ width:100%; height:520px; }}
      h2 {{ font-size:16px; text-align:center; margin:10px 0 6px 0; }}
    </style>
    <script type="text/javascript">
      google.charts.load("current", {{packages:["sankey"]}});
      google.charts.setOnLoadCallback(drawChart);

      function getChartSize() {{
        var el = document.getElementById("sankey_chart");
        var width = el.clientWidth || 640;
        var height = Math.max(420, Math.round(width * 0.85));
        return {{ width: width, height: height }};
      }}

      function drawChart() {{
        var data = new google.visualization.DataTable();
        data.addColumn("string", "Fra");
        data.addColumn("string", "Til");
        data.addColumn("number", "Søkere");

        data.addRows([
          {sankey_rows}
        ]);

        var size = getChartSize();

        var options = {{
          width: size.width,
          height: size.height,
          sankey: {{
            node: {{
              label: {{ fontSize: 12, color: "black" }},
              nodePadding: 10,
              width: 12
            }},
            link: {{
              colorMode: "gradient"
            }}
          }}
        }};

        var chart = new google.visualization.Sankey(document.getElementById("sankey_chart"));
        chart.draw(data, options);
      }}

      window.addEventListener("resize", function() {{
        drawChart();
      }});
    </script>
  </head>
  <body>
    <div id="wrap">
      <div id="sankey_chart"></div>
    </div>
  </body>
</html>
')

# --- 6. Write to file -----------------------------------------------------

# Output-folder
folder_path <- "figurer/side_2_geografi_hc/sankey/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

writeLines(html_template, file.path(folder_path, "sankey_landsdel.html"))



##########################################
### STABLET STOLPEDIAGRAM - HIGHCHARTS ###
##########################################

library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(highcharter)
library(htmlwidgets)

### FORBEREDER DATAENE ------------------------------------------------------

fylker_tilfra <- dt %>%
  filter(
    aar == siste_aar,
    prioritet == 1,
    fylke_soker != "UKJENT"
  ) %>%
  group_by(fylke_soker, studiested_fylke) %>%
  summarise(
    n_applicants = n_distinct(regnr),
    .groups = "drop"
  ) %>%
  mutate(
    hjem_ut = if_else(
      fylke_soker == studiested_fylke,
      "Førstevalg i hjemfylket",
      "Førstevalg utenfor hjemfylket"
    ),
    studiested_landsdel = case_when(
      studiested_fylke %in% c("Troms", "Finnmark", "Nordland") ~ "Nord-Norge",
      studiested_fylke %in% "Trøndelag" ~ "Trøndelag",
      studiested_fylke %in% c("Møre og Romsdal", "Vestland", "Rogaland") ~ "Vestlandet",
      studiested_fylke %in% c(
        "Østfold", "Akershus", "Buskerud", "Telemark",
        "Innlandet", "Vestfold", "Oslo"
      ) ~ "Østlandet",
      studiested_fylke %in% "Agder" ~ "Sørlandet",
      TRUE ~ NA_character_
    )
  )

### LAGER DATA TIL HIGHCHARTS -----------------------------------------------

county_summary <- fylker_tilfra %>%
  filter(
    !is.na(fylke_soker),
    fylke_soker != "NA",
    !fylke_soker %in% c("UKJENT", "Svalbard")
  ) %>%
  group_by(fylke_soker, hjem_ut) %>%
  summarise(
    total_applicants = sum(n_applicants),
    .groups = "drop"
  ) %>%
  group_by(fylke_soker) %>%
  mutate(
    percent = 100 * total_applicants / sum(total_applicants),
    label = paste0(
      comma(total_applicants, big.mark = " "),
      " (",
      number(percent, accuracy = 0.1, decimal.mark = ","),
      " %)"
    )
  ) %>%
  ungroup()

county_summary


source("funksjoner/stablet_stolpe_funksjon.R")
make_stacked_pct_chart(
  data         = county_summary,
  category_var = fylke_soker,
  stack_var    = hjem_ut,
  value_var    = total_applicants,
  order_by     = "Førstevalg i hjemfylket",
  colors       = c(
    "Førstevalg utenfor hjemfylket" = "#0025A0",
    "Førstevalg i hjemfylket"       = "#E72F72"
  ),
  orientation      = "horizontal",
  chart_height     = 700,
  bar_width        = 26,
  label_font_size  = "9px",
  label_threshold  = 8,
  cat_label        = "Fylke",
  stack_label      = "Søknadstype",
  save_path        = "figurer/side_2_geografi_hc/stolpe/sokere_tilfra_stablet_func.html"
)



########################################################################
### TABELLER - BRUKER FERDIG FUNKSJON TIL Å BYGGE TABULERTE TABELLER ###
########################################################################

### Henter inn tabellfunksjon

source("funksjoner/tabell_funksjon.R")


###########################################################
### TIL FRA FYLKER - HJEMSTESFYLKE TIL STUDIESTEDSFYLKE ###
###########################################################

## Forbereder datasettet til tabellene
fylker_tilfra <- dt %>%
  filter(aar == siste_aar, prioritet == 1, fylke_soker != "UKJENT") %>% 
  group_by(fylke_soker, studiested_fylke) %>%
  summarise(n_applicants = n_distinct(fnr), .groups = "drop") %>%
  group_by(fylke_soker) %>%
  mutate(
    percentage_num = 100 * n_applicants / sum(n_applicants),
    percentage = scales::number(percentage_num, accuracy = 0.1, decimal.mark = ","),
    percentage = paste0(percentage, " %"),
    n_applicants = scales::number(
      n_applicants,
      big.mark = " ",
      decimal.mark = ",",
      accuracy = 1
    )
  ) %>%
  ungroup()

fylker_tilfra_clean <- fylker_tilfra %>%
  arrange(fylke_soker, desc(percentage_num)) %>%
  select(fylke_soker, studiested_fylke, n_applicants, percentage_num, percentage)

fylker_tilfra_clean
# Bruker funksjonen for å lage dropdown tables
make_hc_table(
  data = fylker_tilfra_clean,
  group_col = "fylke_soker",
  display_cols = c(
    "Studiested" = "studiested_fylke",
    "Antall" = "n_applicants",
    "Andel" = "percentage"
  ),
  sort_by = "percentage_num",
  sort_desc = TRUE,
  title = "Førstevalgssøkere etter hjemstedsfylke",
  size = "normal",
  export_xlsx = TRUE,
  export_file_stem = "fylker_tilfra",
  out_file = "figurer/side_2_geografi_hc/tabeller/dropdown_fylke_table.html"
)

#####################################################################
### Førstevalgssøkere - fordeling på institusjoner innad i fylket ###
#####################################################################

## Forbereder datasettet til tabellene

fylker_tilfra_inst <- dt %>%
  filter(aar == siste_aar, prioritet == 1) %>%
  group_by(studiested_fylke, inst_navn) %>%
  summarise(n_applicants = n_distinct(fnr), .groups = "drop") %>%
  filter(!is.na(studiested_fylke), studiested_fylke != "UKJENT") %>%
  group_by(studiested_fylke) %>%
  mutate(
    percentage_num = 100 * n_applicants / sum(n_applicants)
  ) %>%
  arrange(studiested_fylke, desc(percentage_num)) %>%  # 🔑 sort HERE
  mutate(
    percentage = scales::number(percentage_num, accuracy = 0.1, decimal.mark = ","),
    percentage = paste0(percentage, " %"),
    n_applicants = scales::number(
      n_applicants,
      big.mark = " ",
      decimal.mark = ",",
      accuracy = 1
    )
  ) %>%
  ungroup()

fylker_tilfra_inst

# Tester funksjonen på institusjonstabellene - FUNKER!
make_hc_table(
  data        = fylker_tilfra_inst,
  group_col   = "studiested_fylke",
  display_cols = c(
    "Studiested" = "inst_navn",
    "Antall"     = "n_applicants",
    "Andel"      = "percentage"
  ),
  sort_by = "percentage_num",
  sort_desc = TRUE,
  title    = "Søkere til institusjoner",
  size = "normal",
  export_xlsx = TRUE,
  export_file_stem = "fylker_tilfra_inst",
  first_col_width = "400px",
  out_file = "figurer/side_2_geografi_hc/tabeller/dropdown_inst_table.html"
)


#######################################################
### Tester tabell uten dropdown-meny fra funksjonen ###
#######################################################


## Forbereder datasettet til tabellene

tilfra_inst <- dt %>%
  filter(aar == siste_aar, prioritet == 1) %>%
  group_by(inst_navn) %>%
  summarise(n_applicants = n_distinct(fnr), .groups = "drop") %>%
  mutate(
    percentage_num = 100 * n_applicants / sum(n_applicants)
  ) %>%
  arrange(desc(percentage_num)) %>%  # 🔑 sort HERE
  mutate(
    percentage = scales::number(percentage_num, accuracy = 0.1, decimal.mark = ","),
    percentage = paste0(percentage, " %"),
    n_applicants = scales::number(
      n_applicants,
      big.mark = " ",
      decimal.mark = ",",
      accuracy = 1
    )
  ) %>%
  ungroup()

tilfra_inst

make_hc_table(
  data = tilfra_inst,
  display_cols = c(
    "Institusjon" = "inst_navn",
    "Antall" = "n_applicants",
    "Andel" = "percentage"
  ),
  sort_by = "percentage_num",
  sort_desc = TRUE,
  title = "Søkere til institusjoner",
  size = "compact",
  export_xlsx = TRUE,
  export_file_stem = "søkere_til_inst",
  first_col_width = "340px",
  out_file = "figurer/side_2_geografi_hc/tabeller/inst_table.html"
)

