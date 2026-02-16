#############################
### SANKEY DIAGRAM - TEST ###
#############################

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
library(tidytext)
library(scales)
library(glue)
library(writexl)

dt <- read_fst("data/so_uhg_moett_master.fst")



# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)

# GLU-utdanninger 
glu_types <- c("Lærerutdanninger - GLU1-7", "Lærerutdanninger - GLU 5-10")

lærerutd <- 
  
  
  lærer <- dt %>% 
  filter(utd_omr == "Lærerutdanninger")

unique(lærer$utd_type)


# Flytter GLU 1-10 på Nord Universitet fra GLU 5-10 til GLU 1-7 (som i DBH)
dt %>% 
  filter(inst_navn=="Nord universitet", utd_omr == "Lærerutdanninger", moett=="J") %>% 
  group_by(programnavn, utd_type) %>% 
  summarise(n = n_distinct(regnr)) %>% 
  print(n=60)

dt <- dt %>%
  mutate(
    utd_type = if_else(
      programnavn == "Grunnskolelærer, 1-10. trinn",
      "Lærerutdanninger - GLU1-7",
      utd_type
    )
  )


## Lærerutdanninger 

fylker_tilfra_laerer <- dt %>%
  filter(aar==siste_aar & moett=="J" & !fylke_soker =="UKJENT", utd_omr == "Lærerutdanninger") %>% 
  group_by(fylke_soker, studiested_fylke) %>%
  summarise(n_applicants = n_distinct(regnr), .groups = "drop")

fylker_tilfra_laerer


#Legge til filtrering på søking til hjemstedsfylke, eller ut av fylket
fylker_tilfra_laerer <- fylker_tilfra_laerer %>%
  mutate(hjem_ut = case_when(
    fylke_soker == studiested_fylke ~ "Møtt i hjemfylket",
    fylke_soker != studiested_fylke ~ "Møtt utenfor hjemfylket"
  ))

# Sjekker koding
fylker_tilfra_laerer

# Legge til kolonne med studiested_landsdel
fylker_tilfra_laerer <- fylker_tilfra_laerer %>%
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
fylker_tilfra_laerer %>% 
  print(n=160)


########################################
### SANKEY DIAGRAM MED GOOGLE CHARTS ###
########################################

# --- 1. Create the new variable: studiested_landsdel ----------------------

fylker_tilfra_laerer <- fylker_tilfra_laerer %>%
  mutate(
    studiested_landsdel = case_when(
      studiested_fylke %in% c("Troms", "Finnmark", "Nordland") ~ "Nord-Norge",
      studiested_fylke %in% c("Trøndelag") ~ "Trøndelag",
      studiested_fylke %in% c("Møre og Romsdal", "Vestland", "Rogaland") ~ "Vestlandet",
      studiested_fylke %in% c("Østfold", "Akershus", "Buskerud", "Telemark",
                              "Innlandet", "Vestfold", "Oslo") ~ "Østlandet",
      studiested_fylke %in% c("Agder") ~ "Sørlandet",
      TRUE ~ NA_character_
    )
  )

# --- 2. Aggregate data for the Sankey diagram -----------------------------

sankey_data <- fylker_tilfra_laerer %>%
  group_by(hjem_ut, studiested_landsdel) %>%
  summarise(value = sum(n_applicants, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(studiested_landsdel))  # remove rows without a matching region

sankey_tallgrunnlag <- sankey_data %>% 
  group_by(hjem_ut, studiested_landsdel) %>% 
  summarise(n = sum(value))

#write.xlsx(sankey_tallgrunnlag, "analyser/GLU_møtte/sankey_tallgrunnlag.xlsx")

# --- 3. Convert to JavaScript row format ---------------------------------

sankey_rows <- sankey_data %>%
  mutate(js_row = paste0("['", hjem_ut, "', '", studiested_landsdel, "', ", value, "]")) %>%
  pull(js_row) %>%
  paste(collapse = ",\n          ")

# --- 4. Define HTML template ---------------------------------------------

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
      <h2>Møtte til lærerutdanninger i 2025 etter landsdel</h2>
      <div id="sankey_chart"></div>
    </div>
  </body>
</html>
')

# --- 5. Write to file -----------------------------------------------------

writeLines(html_template, "figurer/sankey_laerer_landsdel_2.html")


######################################################
### SANKEY MED GOOGLE CHARTS - MED TALL OG ANDELER ###
######################################################

# --- 1. Create the new variable: studiested_landsdel ----------------------

fylker_tilfra_laerer <- fylker_tilfra_laerer %>%
  mutate(
    studiested_landsdel = case_when(
      studiested_fylke %in% c("Troms", "Finnmark", "Nordland") ~ "Nord-Norge",
      studiested_fylke %in% c("Trøndelag") ~ "Trøndelag",
      studiested_fylke %in% c("Møre og Romsdal", "Vestland", "Rogaland") ~ "Vestlandet",
      studiested_fylke %in% c("Østfold", "Akershus", "Buskerud", "Telemark",
                              "Innlandet", "Vestfold", "Oslo") ~ "Østlandet",
      studiested_fylke %in% c("Agder") ~ "Sørlandet",
      TRUE ~ NA_character_
    )
  )

# --- 2. Aggregate data for the Sankey diagram -----------------------------

sankey_data <- fylker_tilfra_laerer %>%
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

writeLines(html_template, "figurer/sankey_laerer_landsdel_alt.html")
