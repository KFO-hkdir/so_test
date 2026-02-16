Sys.setlocale("LC_ALL", "nb_NO.UTF-8")

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(stringr)
library(fst)

# ------------------------
# Load your data
# ------------------------

dt <- read_fst("data/so_uhg_hoved_master.fst")

# Fargeskala
hkdir_farger <- c(
  "#E72F72", "#0025A0", "#EA591D", "#9B3699", "#FF8C43",
  "#A80037", "#FFC948", "#630879", "#000000", "#A2A4A5",
  "#171B4E", "#E72F72", "#0025A0", "#9B3699", "#FFC948", "#A2A4A5"
)

# ------------------------
# Helper functions
# ------------------------

make_safe_filename <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^0-9A-Za-z_-]", "_", x)
}

make_summary <- function(dt_filtered) {
  sokere_aar <- dt_filtered %>%
    filter(status == "AKT") %>%
    group_by(aar) %>%
    summarise(n_sokere = n_distinct(regnr), .groups = "drop")
  
  studplass_aar <- dt_filtered %>%
    filter(status == "AKT") %>%
    group_by(aar) %>%
    distinct(studiekode, studieplasser) %>%
    summarise(antall_studieplasser = sum(studieplasser, na.rm = TRUE), .groups = "drop")
  
  kvalifisert_aar <- dt_filtered %>%
    filter(status == "AKT") %>%
    group_by(aar, regnr) %>%
    summarise(is_qualified = any(kvalifisert == "J"), .groups = "drop_last") %>%
    summarise(kvalifiserte_søkere = sum(is_qualified), .groups = "drop")
  
  tilbud_aar <- dt_filtered %>%
    filter(tilbud == "J") %>%
    group_by(aar) %>%
    summarise(tilbud = n_distinct(regnr), .groups = "drop")
  
  tilbud_forste_aar <- dt_filtered %>%
    filter(prioritet == 1 & tilbud == "J") %>%
    group_by(aar) %>%
    summarise(tilbud_førstevalg = n_distinct(regnr), .groups = "drop")
  
  sokere_aar %>%
    left_join(kvalifisert_aar, by = "aar") %>%
    left_join(studplass_aar, by = "aar") %>%
    left_join(tilbud_aar, by = "aar") %>%
    left_join(tilbud_forste_aar, by = "aar") %>%
    mutate(across(-aar, ~replace_na(.x, 0))) %>%
    pivot_longer(-aar, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(
      variable,
      levels = c(
        "n_sokere",
        "kvalifiserte_søkere",
        "antall_studieplasser",
        "tilbud",
        "tilbud_førstevalg"
      ),
      labels = c(
        "Søkere",
        "Kvalifiserte søkere",
        "Studieplasser",
        "Søkere med tilbud",
        "Søkere med tilbud (førstevalg)"
      )
    ))
}

# ------------------------
# Generate plots
# ------------------------

utd_values <- unique(dt$utd_omr)
plots <- list()

folder_path <- "figurer/utd_omr_tab/"
dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)

for (utd in utd_values) {
  
  dt_rs <- make_summary(dt %>% filter(utd_omr == utd))
  max_val <- max(dt_rs$value, na.rm = TRUE)
  if (max_val <= 0) max_val <- 1
  
  p <- ggplot(dt_rs, aes(aar, value, color = variable, group = variable)) +
    geom_line(size = 1.5) +
    geom_point(aes(text = paste(variable, ":", scales::comma(value))), size = 3) +
    scale_color_manual(values = hkdir_farger) +
    scale_y_continuous(limits = c(0, max_val * 1.05)) +
    scale_x_continuous(breaks = unique(dt_rs$aar)) +
    labs(title = paste("Utdanningsområde:", utd), x = "", y = "") +
    theme_minimal(base_size = 14)
  
  plots[[utd]] <- ggplotly(p, tooltip = "text") %>%
    layout(
      height = 800,
      showlegend = FALSE   # ✅ disable Plotly legend
    )
  
  saveWidget(
    plots[[utd]],
    file = file.path(folder_path, paste0(make_safe_filename(utd), ".html")),
    selfcontained = TRUE
  )
}

# ------------------------
# Shared legend (HTML)
# ------------------------

legend_items <- data.frame(
  label = c(
    "Søkere",
    "Kvalifiserte søkere",
    "Studieplasser",
    "Søkere med tilbud",
    "Søkere med tilbud (førstevalg)"
  ),
  color = hkdir_farger[1:5]
)

shared_legend <- tags$div(
  style = "
    display:flex;
    flex-wrap:wrap;
    gap:20px;
    margin-bottom:15px;
    font-size:14px;
  ",
  lapply(seq_len(nrow(legend_items)), function(i) {
    tags$div(
      style = "display:flex; align-items:center; gap:6px;",
      tags$span(style = paste0(
        "width:12px;height:12px;display:inline-block;background:",
        legend_items$color[i]
      )),
      tags$span(legend_items$label[i])
    )
  })
)

# ------------------------
# Generate tabbed interface (iframes)
# ------------------------

tabs <- list()
tab_panels <- list()

for (i in seq_along(utd_values)) {
  
  utd <- utd_values[i]
  
  tabs[[i]] <- tags$li(
    class = ifelse(i == 1, "active", ""),
    tags$a(
      `data-toggle` = "tab",
      href = paste0("#tab", i),
      utd
    )
  )
  
  tab_panels[[i]] <- tags$div(
    class = paste("tab-pane fade", ifelse(i == 1, "in active", "")),
    id = paste0("tab", i),
    tags$iframe(
      src = paste0(make_safe_filename(utd), ".html"),
      style = "width:100%; height:800px; border:none;"
    )
  )
}

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$link(
      rel = "stylesheet",
      href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"
    ),
    tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.min.js"),
    tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  tags$body(
    tags$div(
      class = "container",
      tags$h2("Interaktive linjediagrammer per fagområde"),
      tags$ul(class = "nav nav-tabs", tabs),
      tags$div(class = "tab-content", tab_panels),
      shared_legend                 # ✅ shared legend here
    )
  )
)

save_html(page, file.path(folder_path, "tabbed_interface_resize.html"))
