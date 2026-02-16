
##################################################
### HOVEDOPPTAK HYU - KART OG GEOGRAFI ###########
##################################################

Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(csmaps)
library(csdata)
library(ggtext)
library(ggmap)
library(scico)
library(fst)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(patchwork)
library(htmltools)
library(gt)

library(htmlwidgets)
library(htmltools)

dt <- read_fst("data/so_uhg_hoved_master.fst")


# Fargeskala
hkdir_farger <- c("#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43","#A80037",
                  
                  "#FFC948", "#630879",  "#000000", "#A2A4A5", "#171B4E",
                  
                  "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5")

# Siste år i data
siste_aar <- max(dt$aar, na.rm = TRUE)



##############################
### NORGESKART MED TOOLTIP ###
##############################


### SO-DATA - TIL TOOLTIP (siste_aar) ###

# Søkere fra:
sokere_fra <- dt %>% 
  filter(aar == siste_aar & status=="AKT") %>% 
  group_by(fylke_soker) %>% 
  summarise(n_sokere_fra = n_distinct(regnr)) %>% 
  bind_rows(
    summarise(., fylke_soker = "Total", n_sokere_fra = sum(n_sokere_fra))
  )

# Søkere til (førstevalg):
sokere_til <- dt %>% 
  filter(aar == siste_aar & prioritet == 1 & status=="AKT") %>% 
  group_by(studiested_fylke) %>% 
  summarise(n_sokere_til = n_distinct(regnr)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_sokere_til = sum(n_sokere_til))
  )

# Søkere tilbud til:
sokere_tilbud_til <- dt %>% 
  filter(aar == siste_aar & tilbud == "J") %>% 
  group_by(studiested_fylke) %>% 
  summarise(n_sokere_tilbud_til = n_distinct(regnr)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_sokere_tilbud_til = sum(n_sokere_tilbud_til))
  )

# Antall institusjoner:
n_inst <- dt %>% 
  filter(aar == siste_aar & status=="AKT") %>% 
  group_by(studiested_fylke) %>% 
  summarise(n_inst = n_distinct(inst_nr)) 

# Antall studier:
n_stud <- dt %>% 
  filter(aar == siste_aar & status=="AKT") %>% 
  group_by(studiested_fylke) %>% 
  summarise(n_stud = n_distinct(studiekode)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_stud = sum(n_stud))
  )
n_stud
# Planlagte studieplasser:
n_studplass <- dt %>% 
  filter(aar == siste_aar  & status=="AKT") %>% 
  group_by(studiested_fylke) %>% 
  distinct(studiekode, studieplasser) %>%
  summarise(n_studplass = sum(studieplasser, na.rm = TRUE)) %>% 
  bind_rows(
    summarise(., studiested_fylke = "Total", n_studplass = sum(n_studplass))
  )
n_studplass


# Merge all "inst_fylke" based summaries
dt_tooltip <- sokere_til %>%
  left_join(sokere_tilbud_til, by = "studiested_fylke") %>% 
  left_join(n_inst, by = "studiested_fylke") %>%
  left_join(n_stud, by = "studiested_fylke") %>%
  left_join(n_studplass, by = "studiested_fylke") 

# Add "Søkere fra" (only matching fylke with inst_fylke)
dt_tooltip <- dt_tooltip %>%
  left_join(sokere_fra, by = c("studiested_fylke" = "fylke_soker"))

dt_tooltip <- dt_tooltip %>%
  mutate(soker_pr_plass = round(n_sokere_til/n_studplass,1))


# Display final data
print(dt_tooltip)





############################################
### KART - FORBEREDER DATASETT M/TOOLTIP ###
############################################

### BRUK DENNE? VINNER! 

### KART - OPPSETT ###


# Hente navn på fylker
county_names <- data.frame(
  location_code = c("county_nor01", "county_nor02", "county_nor03", "county_nor11", "county_nor15",
                    "county_nor18", "county_nor33", "county_nor34", "county_nor39", "county_nor40",
                    "county_nor42", "county_nor46", "county_nor50", "county_nor55", "county_nor56"),
  location_name = c("Østfold", "Akershus", "Oslo", "Rogaland", "Møre og Romsdal",
                    "Nordland", "Buskerud", "Innlandet", "Vestfold", "Telemark",
                    "Agder", "Vestland", "Trøndelag", "Troms", "Finnmark")
)


# View mapping
print(county_names)


# 🎯 Use a high-resolution dataset instead of `map_df`
map_sf <- csmaps::nor_county_map_b2024_default_sf %>%
  left_join(county_names, by = "location_code") %>%
  left_join(dt_tooltip, by = c("location_name" = "studiested_fylke"))

# 🌍 Define color palette
pal <- colorFactor(RColorBrewer::brewer.pal(n = 8, "Dark2"), domain = map_sf$location_name)




#############################
### Kart - gammel versjon ###
#############################


# 🗺️ Create a clean, high-quality interactive map
kart_soking_uhg <- leaflet(map_sf, options = leafletOptions(minZoom = 4.5, maxZoom = 5.5)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%  # Minimalist base map
  setView(lng = 10, lat = 64, zoom = 5) %>%  # Center on Norway
  addPolygons(
    fillColor = "#F58220",
    color = "white",  # Clean county borders
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    smoothFactor = 0.5,  # Makes coastal lines smoother
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<b style='font-size:14px;'>", location_name, "</b><br>",
      "<span style='font-size:12px;'>Søkere til (førstevalg): ", n_sokere_til, "</span><br>",
      "<span style='font-size:12px;'>Søkere tilbud til: ", n_sokere_tilbud_til, "</span><br>",
      "<span style='font-size:12px;'>Søkere fra: ", n_sokere_fra, "</span><br>",
      "<span style='font-size:12px;'>Antall institusjoner: ", n_inst, "</span><br>",
      "<span style='font-size:12px;'>Antall studier: ", n_stud, "</span><br>",
      "<span style='font-size:12px;'>Planlagte studieplasser: ", n_studplass, "</span>"
    ) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "color" = "black"),
      direction = "auto"
    )
  ) #%>%
# addLegend(
#   pal = pal,
#   values = ~location_name,
#   opacity = 0.7,
#   title = "Fylker (2024)",
#   position = "bottomright"
# )

htmlwidgets::saveWidget(
  widget = kart_soking_uhg,
  file = "kart/kart_soking_uhg.html",
  selfcontained = TRUE
)


######################################################
### TESTER ALTERNATIVE VARIANTER - TESTING GROUNDS ###
######################################################


### STATISK "LÅST" STØRRELSE ###


# 🗺️ Create a clean, high-quality interactive map
kart_soking_uhg <- leaflet(map_sf, options = leafletOptions(
  zoomControl = FALSE,
  dragging = FALSE,
  scrollWheelZoom = FALSE,
  doubleClickZoom = FALSE,
  boxZoom = FALSE,
  keyboard = FALSE,
  touchZoom = FALSE,
  tap = FALSE
  )) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%  # Minimalist base map
  setView(lng = 18, lat = 61, zoom = 5) %>%  # Center on Norway
  addPolygons(
    fillColor = "#F58220",
    color = "white",  # Clean county borders
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    smoothFactor = 0.5,  # Makes coastal lines smoother
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<b style='font-size:14px;'>", location_name, "</b><br>",
      "<span style='font-size:12px;'>Søkere til (førstevalg): ", n_sokere_til, "</span><br>",
      "<span style='font-size:12px;'>Søkere tilbud til: ", n_sokere_tilbud_til, "</span><br>",
      "<span style='font-size:12px;'>Søkere fra: ", n_sokere_fra, "</span><br>",
      "<span style='font-size:12px;'>Antall institusjoner: ", n_inst, "</span><br>",
      "<span style='font-size:12px;'>Antall studier: ", n_stud, "</span><br>",
      "<span style='font-size:12px;'>Planlagte studieplasser: ", n_studplass, "</span>"
    ) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "color" = "black"),
      direction = "auto"
    )
  ) #%>%
# addLegend(
#   pal = pal,
#   values = ~location_name,
#   opacity = 0.7,
#   title = "Fylker (2024)",
#   position = "bottomright"
# )

htmlwidgets::saveWidget(
  widget = kart_soking_uhg,
  file = "kart/kart_soking_uhg.html",
  selfcontained = TRUE
)


### VERSJON MED JUSTERING UAVHENGIG AV IFRAME-STØRRELSE ###


# 🗺️ Create a clean, high-quality interactive map
kart_soking_uhg <- leaflet(map_sf, 
                           height = 800,
                           options = leafletOptions(
  zoomControl = FALSE,
  dragging = FALSE,
  scrollWheelZoom = FALSE,
  doubleClickZoom = FALSE,
  boxZoom = FALSE,
  keyboard = FALSE,
  touchZoom = FALSE,
  tap = FALSE
)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%  # Minimalist base map
  setView(lng = 18, lat = 65, zoom = 5) %>%  # Center on Norway
  addPolygons(
    fillColor = "#F58220",
    color = "white",  # Clean county borders
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    smoothFactor = 0.5,  # Makes coastal lines smoother
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<b style='font-size:14px;'>", location_name, "</b><br>",
      "<span style='font-size:12px;'>Søkere til (førstevalg): ", n_sokere_til, "</span><br>",
      "<span style='font-size:12px;'>Søkere tilbud til: ", n_sokere_tilbud_til, "</span><br>",
      "<span style='font-size:12px;'>Søkere fra: ", n_sokere_fra, "</span><br>",
      "<span style='font-size:12px;'>Antall institusjoner: ", n_inst, "</span><br>",
      "<span style='font-size:12px;'>Antall studier: ", n_stud, "</span><br>",
      "<span style='font-size:12px;'>Planlagte studieplasser: ", n_studplass, "</span>"
    ) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "color" = "black"),
      direction = "auto"
    )
  ) #%>%
# addLegend(
#   pal = pal,
#   values = ~location_name,
#   opacity = 0.7,
#   title = "Fylker (2024)",
#   position = "bottomright"
# )

htmlwidgets::saveWidget(
  widget = kart_soking_uhg,
  file = "kart/kart_soking_uhg.html",
  selfcontained = TRUE
)








################################ KART ##########################################

#######################################
#### STATISKE KART TIL GRID-VISNING ###
#######################################


### SØKERE TIL FYLKET ### 

pal_til <- colorNumeric(
  palette = colorRampPalette(brewer.pal(9, "Greens"))(20),  
  domain = map_sf$n_sokere_til,
  na.color = "transparent"
)

kart_sokere_til_uhg <- leaflet(map_sf, options = leafletOptions(
  minZoom = 2.5, 
  maxZoom = 5.5
)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lng = 16, lat = 65, zoom = 3.5) %>%
  addPolygons(
    fillColor = ~pal_til(n_sokere_til),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
    label = ~paste0("<b>", location_name, "</b><br>Søkere til: ", n_sokere_til) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(style = list("font-size" = "14px"), direction = "auto")
  ) %>%
  addLegend(pal = pal_til, values = ~n_sokere_til, title = "Søkere til", position = "bottomright") 

kart_sokere_til_uhg

### SØKERE FRA FYLKET ### 

pal_fra <- colorNumeric(
  palette = colorRampPalette(brewer.pal(9, "Purples"))(20),  # Orange shades
  domain = map_sf$n_sokere_fra,
  na.color = "transparent"
)

kart_sokere_fra_uhg <- leaflet(map_sf, options = leafletOptions(
  minZoom = 2.5, 
  maxZoom = 5.5
)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lng = 16, lat = 65, zoom = 3.5) %>%
  addPolygons(
    fillColor = ~pal_fra(n_sokere_fra),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
    label = ~paste0("<b>", location_name, "</b><br>Søkere fra: ", n_sokere_fra) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(style = list("font-size" = "14px"), direction = "auto")
  ) %>%
  addLegend(pal = pal_fra, values = ~n_sokere_fra, title = "Søkere fra", position = "bottomright")

kart_sokere_fra_uhg

### ANTALL PLANLAGTE STUDIEPLASSER I FYLKENE ### 

pal_studplass <- colorNumeric(
  palette = colorRampPalette(brewer.pal(9, "Blues"))(20),  # Blue shades
  domain = map_sf$n_studplass,
  na.color = "transparent"
)

kart_studplass_uhg <- leaflet(map_sf, options = leafletOptions(
  minZoom = 2.5, 
  maxZoom = 5.5
)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lng = 16, lat = 65, zoom = 3.5) %>%
  addPolygons(
    fillColor = ~pal_studplass(n_studplass),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
    label = ~paste0("<b>", location_name, "</b><br>Studieplasser: ", n_studplass) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(style = list("font-size" = "14px"), direction = "auto")
  ) %>%
  addLegend(pal = pal_studplass, values = ~n_studplass, title = "Studieplasser", position = "bottomright")

kart_studplass_uhg

### FYLKENE MED HØYEST ANTALL SØKERE PER PLANLAGTE STUDIEPLASS ### 

pal_studplass <- colorNumeric(
  palette = colorRampPalette(brewer.pal(9, "Oranges"))(20),  # Blue shades
  domain = map_sf$soker_pr_plass,
  na.color = "transparent"
)

kart_sokerperplass_uhg <- leaflet(map_sf, options = leafletOptions(
  minZoom = 2.5, 
  maxZoom = 5.5
)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lng = 16, lat = 65, zoom = 3.5) %>%
  addPolygons(
    fillColor = ~pal_studplass(soker_pr_plass),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
    label = ~paste0("<b>", location_name, "</b><br>Søkere pr. plass: ", soker_pr_plass) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(style = list("font-size" = "14px"), direction = "auto")
  ) %>%
  addLegend(pal = pal_studplass, values = ~soker_pr_plass, title = "Søkere pr. plass", position = "bottomright")

kart_sokerperplass_uhg

### FYLKENE MED FLEST SØKERE MED TILBUD ### 

pal_studplass <- colorNumeric(
  palette = colorRampPalette(brewer.pal(9, "Reds"))(20),  # Blue shades
  domain = map_sf$n_sokere_tilbud_til,
  na.color = "transparent"
)

kart_sokertilbud_uhg <- leaflet(map_sf, options = leafletOptions(
  minZoom = 2.5, 
  maxZoom = 5.5
)) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  setView(lng = 16, lat = 65, zoom = 3.5) %>%
  addPolygons(
    fillColor = ~pal_studplass(n_sokere_tilbud_til),
    color = "white",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
    label = ~paste0("<b>", location_name, "</b><br>Søkere med tilbud: ", n_sokere_tilbud_til) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(style = list("font-size" = "14px"), direction = "auto")
  ) %>%
  addLegend(pal = pal_studplass, values = ~n_sokere_tilbud_til, title = "Søkere med tilbud", position = "bottomright")

kart_sokertilbud_uhg



### GRID-VISNING ### 

### Alternativ med htmltools i stedet for Shiny 

map_grid <- tagList(
  tags$div(
    style = "display: flex; justify-content: space-around;",
    tags$div(style = "width: 33%;", kart_sokere_til_uhg),
    tags$div(style = "width: 33%;", kart_sokere_fra_uhg),
    tags$div(style = "width: 33%;", kart_studplass_uhg),
    tags$div(style = "width: 33%;", kart_sokertilbud_uhg)
  )
)

map_grid
saveRDS(map_grid, "figurer_tabeller/3_geografi/kart_grid_3_uhg.rds")


map_grid_12 <- tagList(
  tags$div(
    style = "display: flex; justify-content: space-around;",
    tags$div(style = "width: 50%;", kart_sokere_til_uhg),
    tags$div(style = "width: 50%;", kart_sokere_fra_uhg)
  )
)


map_grid_34 <- tagList(
  tags$div(
    style = "display: flex; justify-content: space-around;",
    tags$div(style = "width: 50%;", kart_studplass_uhg),
    tags$div(style = "width: 50%;", kart_sokertilbud_uhg)
  )
)

map_grid_12
saveRDS(map_grid_12, "figurer_tabeller/3_geografi/map_grid_12_uhg.rds")
map_grid_34
saveRDS(map_grid_34, "figurer_tabeller/3_geografi/map_grid_34_uhg.rds")




############################# FIGURER / TABELLER ###############################


#######################################################################
### TABELLER MED SØKRE MED TILBUD TIL/FRA FOR HVERT HJEMSTEDSFYLKE ####
#######################################################################


fylker_tilfra <- dt %>%
  filter(aar==siste_aar & tilbud=="J") %>% 
  group_by(fylke_soker, studiested_fylke) %>%
  summarise(n_applicants = n_distinct(regnr), .groups = "drop")

fylker_tilfra %>% 
  print(n=50)


fylker_tilfra <- fylker_tilfra %>%
  group_by(fylke_soker) %>%
  mutate(percentage = paste0(format(round((n_applicants / sum(n_applicants)) * 100, 1),decimal.mark = ","), "%")) %>%
  ungroup()

fylker_tilfra %>% 
  print(n=50)

# Function to generate formatted gt tables for each fylke
create_gt_table <- function(fylke_name) {
  fylker_tilfra %>%
    filter(fylke_soker == fylke_name) %>%
    arrange(desc(n_applicants)) %>%  # Sort by the number of applicants
    gt() %>%
    tab_header(title = md(paste0("**Søkere fra ", fylke_name, "**"))) %>%
    cols_label(
      fylke_soker = "Hjemstedsfylke",
      studiested_fylke = "Studiested (fylke)",
      n_applicants = "Antall søkere med tilbud",
      percentage = "Andel (%)"
    ) %>%
    fmt_number(columns = n_applicants, decimals = 0) %>%  # Format numbers without decimals
    fmt_number(columns = percentage, decimals = 1) %>%  # Keep 1 decimal for percentage
    cols_align(align = "right", columns = c(n_applicants, percentage)) %>%
    tab_options(
      table.width = pct(100),
      column_labels.font.weight = "bold"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#F58220"),  # Orange background
        cell_text(color = "white", weight = "bold", align = "right")
      ),
      locations = cells_column_labels(columns = c(n_applicants, percentage))
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#F58220"),  # Orange background
        cell_text(color = "white", weight = "bold", align = "left")
      ),
      locations = cells_column_labels(columns = c(fylke_soker, studiested_fylke)) 
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold", align = "left")
      ),
      locations = cells_body(columns = fylke_soker)
    ) %>% 
    opt_row_striping() %>%
    tab_options(
      row.striping.background_color = "#fde9d9"  # Light orange striping
    ) %>%
    tab_options(
      table.width = pct(100),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F58220"
    )
}

# Generate a list of tables for each fylke
fylke_tables <- lapply(unique(fylker_tilfra$fylke_soker), create_gt_table)


# Save each generated table in fylke_tables as an rds
# Define save path
save_path <- "figurer_tabeller/3_geografi/"

# Loop through fylke_tables and save each with correct name
for (i in seq_along(fylke_tables)) {
  
  # Extract the first table in list
  first_table <- fylke_tables[[i]]
  
  # Convert gt table to dataframe
  first_table_df <- as.data.frame(first_table)
  
  # Extract the first value in the first column (the fylke name)
  fylke_name <- first_table_df %>% slice(1) %>% pull(1)
  
  # Generate object name dynamically (replace spaces with underscores, lowercase)
  object_name <- paste0("sokere_", tolower(gsub(" ", "_", fylke_name)), "_tabell")
  
  # Assign table to dynamically named variable
  assign(object_name, first_table)
  
  # Save as RDS file
  saveRDS(first_table, file = paste0(save_path, object_name, "_uhg.rds"))
}


# Display tables tables

oslo <- readRDS("figurer_tabeller/3_geografi//sokere_oslo_tabell_uhg.rds")
oslo



####### Søkere tilbud til institusjoner (studiested) innad i hvert fylke #######

fylker_tilfra_inst <- dt %>%
  filter(aar==siste_aar & tilbud=="J") %>% 
  group_by(studiested_fylke, inst_navn) %>%
  summarise(n_applicants = n_distinct(regnr), .groups = "drop")

fylker_tilfra_inst


fylker_tilfra_inst <- fylker_tilfra_inst %>%
  group_by(studiested_fylke) %>%
  mutate(percentage = paste0(format(round((n_applicants / sum(n_applicants)) * 100, 1),decimal.mark = ","), "%")) %>%
  ungroup()



fylker_tilfra_inst %>% 
  print(n=45)


# Function to generate formatted gt tables for each fylke
create_gt_table_inst <- function(fylke_name) {
  fylker_tilfra_inst %>%
    filter(studiested_fylke == fylke_name) %>%
    arrange(desc(n_applicants)) %>%  # Sort by the number of applicants
    gt() %>%
    tab_header(title = md(paste0("**Søkere med tilbud i ", fylke_name, "**"))) %>%
    cols_label(
      studiested_fylke = "Studiestedets fylke",
      inst_navn = "Institusjonsnavn",
      n_applicants = "Antall søkere med tilbud",
      percentage = "Andel (%)"
    ) %>%
    fmt_number(columns = n_applicants, decimals = 0) %>%  # Format numbers without decimals
    fmt_number(columns = percentage, decimals = 1) %>%  # Keep 1 decimal for percentage
    cols_align(align = "right", columns = c(n_applicants, percentage)) %>%
    tab_options(
      table.width = pct(100),
      column_labels.font.weight = "bold"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#F58220"),  # Orange background
        cell_text(color = "white", weight = "bold", align = "left")
      ),
      locations = cells_column_labels(columns = c(studiested_fylke, inst_navn))
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#F58220"),  # Orange background
        cell_text(color = "white", weight = "bold", align = "right")
      ),
      locations = cells_column_labels(columns = c(n_applicants, percentage))
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold", align = "left")
      ),
      locations = cells_body(columns = studiested_fylke)
    ) %>% 
    opt_row_striping() %>%
    tab_options(
      row.striping.background_color = "#fde9d9"  # Light orange striping
    ) %>%
    tab_options(
      table.width = pct(100),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F58220"
    )
}

# Generate a list of tables for each fylke
fylke_tables_inst <- lapply(unique(fylker_tilfra_inst$studiested_fylke), create_gt_table_inst)

fylke_tables_inst[2]

# Save each generated table in fylke_tables as an rds
# Define save path
save_path <- "figurer_tabeller/3_geografi/"

# Loop through fylke_tables and save each with correct name
for (i in seq_along(fylke_tables_inst)) {
  
  # Extract the first table in list
  first_table <- fylke_tables_inst[[i]]
  
  # Convert gt table to dataframe
  first_table_df <- as.data.frame(first_table)
  
  # Extract the first value in the first column (the fylke name)
  fylke_name <- first_table_df %>% slice(1) %>% pull(1)
  
  # Generate object name dynamically (replace spaces with underscores, lowercase)
  object_name <- paste0("sokere_", tolower(gsub(" ", "_", fylke_name)), "_inst_tabell")
  
  # Assign table to dynamically named variable
  assign(object_name, first_table)
  
  # Save as RDS file
  saveRDS(first_table, file = paste0(save_path, object_name, "_uhg.rds"))
}


# Display tables tables
oslo_inst <- readRDS("figurer_tabeller/3_geografi/sokere_oslo_inst_tabell_uhg.rds")
oslo_inst

####### Søkere tilbud til institusjoner med andeler - nasjonalt #######



tilbud_inst <- dt %>%
  filter(aar==siste_aar & tilbud=="J") %>% 
  group_by(inst_navn) %>%
  summarise(n_applicants = n_distinct(regnr), .groups = "drop") %>% 
  mutate(percentage = paste0(format(round((n_applicants / sum(n_applicants)) * 100, 1),decimal.mark = ","), "%")) %>%
  print(n=45)


# Lager tabell
tilbud_inst_tabell <- tilbud_inst %>%
  arrange(desc(n_applicants)) %>%  # Sort by the number of applicants
  gt() %>%
  tab_header(title = md(paste0("**Søkere med tilbud per lærested**"))) %>%
  cols_label(
    inst_navn = "Institusjonsnavn",
    n_applicants = "Antall søkere med tilbud",
    percentage = "Andel (%)"
  ) %>%
  fmt_number(columns = n_applicants, decimals = 0) %>%  # Format numbers without decimals
  fmt_number(columns = percentage, decimals = 1) %>%  # Keep 1 decimal for percentage
  cols_align(align = "right", columns = c(n_applicants, percentage)) %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F58220"),  # Orange background
      cell_text(color = "white", weight = "bold", align = "left")
    ),
    locations = cells_column_labels(columns = c(inst_navn))
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#F58220"),  # Orange background
      cell_text(color = "white", weight = "bold", align = "right")
    ),
    locations = cells_column_labels(columns = c(n_applicants, percentage))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "left")
    ),
    locations = cells_body(columns = inst_navn)
  ) %>% 
  opt_row_striping() %>%
  tab_options(
    row.striping.background_color = "#fde9d9"  # Light orange striping
  ) %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.weight = "bold",
    column_labels.background.color = "#F58220"
  )

tilbud_inst_tabell
saveRDS(tilbud_inst_tabell, "figurer_tabeller/3_geografi/tilbud_inst_tabell_uhg.rds")

#############################################################################
###  PAI OG SØYLEDIAGRAM SOM VISER SØKERE INN ELLER UT AV HJEMSTEDSFYLKET ###
#############################################################################

fylker_tilfra <- dt %>%
  filter(aar==siste_aar & tilbud=="J" & !fylke_soker =="UKJENT") %>% 
  group_by(fylke_soker, studiested_fylke) %>%
  summarise(n_applicants = n_distinct(regnr), .groups = "drop")

fylker_tilfra


#Legge til filtrering på søking til hjemstedsfylke, eller ut av fylket
fylker_tilfra <- fylker_tilfra %>%
  mutate(hjem_ut = case_when(
    fylke_soker == studiested_fylke ~ "Tilbud i hjemfylket",
    fylke_soker != studiested_fylke ~ "Tilbud utenfor hjemfylket"
  ))

# Sjekker koding
fylker_tilfra


### Filtrering på alder - bare sjekker! 

dt %>% 
  filter(aar==siste_aar & prioritet==1) %>% 
  mutate(hjem_ut = case_when(
    fylke_soker == studiested_fylke ~ "Tilbud i hjemfylket",
    fylke_soker != studiested_fylke ~ "Tilbud utenfor hjemfylket"
  )) %>% 
  group_by(hjem_ut) %>% 
  summarise(snittalder = mean(alder))


### PAI ### 
# Aggregate data for the pie chart
overall_summary <- fylker_tilfra %>%
  group_by(hjem_ut) %>%
  summarise(total_applicants = sum(n_applicants), .groups = "drop") %>%
  mutate(percentage = round(total_applicants / sum(total_applicants) * 100, 1),
         label = paste0(total_applicants, " (", percentage, "%)"))  # Create label with count + percentage
overall_summary
# Create pie chart with white labels
tilbud_tilfra_pai_uhg <- ggplot(overall_summary, aes(x = "", y = total_applicants, fill = hjem_ut)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5, fontface = "bold", color = "white") +  # White labels
  scale_fill_manual(values = c("Tilbud i hjemfylket" = "#E72F72", "Tilbud utenfor hjemfylket" = "#0025A0")) +
  labs(title = "Andel søkere som har tilbud innenfor vs. utenfor hjemfylket", fill = "") +
  theme_void() +  
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

tilbud_tilfra_pai_uhg
saveRDS(tilbud_tilfra_pai_uhg, "figurer_tabeller/3_geografi/tilbud_tilfra_pai_uhg.rds")



### STABLET SØYLE - PER FYLKE ### 


fylker_tilfra <- dt %>%
  filter(aar==siste_aar & tilbud=="J" & !fylke_soker =="UKJENT") %>% 
  group_by(fylke_soker, studiested_fylke) %>%
  summarise(n_applicants = n_distinct(regnr), .groups = "drop")

fylker_tilfra


#Legge til filtrering på søking til hjemstedsfylke, eller ut av fylket
fylker_tilfra <- fylker_tilfra %>%
  mutate(hjem_ut = case_when(
    fylke_soker == studiested_fylke ~ "Tilbud i hjemfylket",
    fylke_soker != studiested_fylke ~ "Tilbud utenfor hjemfylket"
  ))

# Sjekker koding
fylker_tilfra


# Aggregate data for the bar chart
county_summary <- fylker_tilfra %>%
  filter(!is.na(fylke_soker) & fylke_soker != "NA" & !fylke_soker %in% c("UKJENT", "Svalbard")) %>%  # Exclude both true NA and character "NA"
  group_by(fylke_soker, hjem_ut) %>%
  summarise(total_applicants = sum(n_applicants), .groups = "drop") %>%
  group_by(fylke_soker) %>%
  mutate(percentage = round(total_applicants / sum(total_applicants) * 100, 1),
         label = paste0(total_applicants, " (", round(percentage, 1), "%)"))

county_summary


county_summary %>% 
  filter(is.na(fylke_soker)) # sjekker om det er missing 

# Ensure all fylker are included, even those without "Søker til hjemfylket"
county_order <- county_summary %>%
  filter(hjem_ut == "Tilbud i hjemfylket") %>%
  arrange(percentage) %>%
  pull(fylke_soker)

# Include any fylker missing from county_order (those that only have "Søker ut av fylket")
all_fylker <- unique(county_summary$fylke_soker)
missing_fylker <- setdiff(all_fylker, county_order)

# Append missing fylker to county_order
county_order <- c(county_order, missing_fylker)

# Convert fylke to factor with complete ordering
county_summary <- county_summary %>%
  mutate(fylke_soker = factor(fylke_soker, levels = county_order))

# Check if any NA values remain
county_summary %>% filter(is.na(fylke_soker))

county_summary %>% 
  filter(fylke_soker=="UKJENT" | fylke_soker=="Svalbard")

county_summary <- county_summary %>%
  mutate(hjem_ut = factor(hjem_ut, levels = c("Tilbud utenfor hjemfylket", "Tilbud i hjemfylket")))  # Reverse stacking order

# Sjekker
county_summary

# Create stacked bar chart with percentage-based x-axis
tilbud_tilfra_soyle_uhg <- ggplot(county_summary, aes(x = fylke_soker, y = percentage, fill = hjem_ut)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4, fontface = "bold", color = "white") +  # White labels
  coord_flip() +  
  scale_fill_manual(values = c("Tilbud utenfor hjemfylket" = "#0025A0","Tilbud i hjemfylket" = "#E72F72"),
                    guide = guide_legend(reverse = TRUE)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +  # Show percentages from 0% to 100%
  labs(title = "Andel søkere med tilbud til hjemstedsfylke vs. ut av fylket",
       x = "", y = "Andel søkere (%)", fill = "Søknadstype") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.y = element_text(size = 12))

tilbud_tilfra_soyle_uhg
saveRDS(tilbud_tilfra_soyle_uhg, "figurer_tabeller/3_geografi/tilbud_tilfra_soyle_uhg.rds")