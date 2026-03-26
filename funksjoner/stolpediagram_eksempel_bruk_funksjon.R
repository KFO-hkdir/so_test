


source("funksjoner/stablet_stolpe_funksjon.R")

### KJØNNSFORDELING FØRSTEVALGSSØKERE PER UTDANNINGSOMRÅDE ###

# ── Prepare data (same as your ggplot pipeline) ────────────────────────────
utdomr_kjonn_data <- dt %>%
  filter(aar == siste_aar, prioritet == 1) %>%
  group_by(utd_omr, kjonn) %>%
  summarise(n = n_distinct(regnr), .groups = "drop")

# ── Call the function ──────────────────────────────────────────────────────
utdomr_kjonn_hc <- make_stacked_pct_chart(
  data         = utdomr_kjonn_data,
  category_var = utd_omr,
  stack_var    = kjonn,
  value_var    = n,
  cat_label = "Utdanningsområde",
  stack_label = "Kjønn",
  order_by     = "Mann",          # sort bars ascending by male share
  colors       = c(
    "Mann"   = "#0D47A1",
    "Kvinne" = "#E91E63"
  ),
  orientation  = "horizontal",
  chart_height = 700,
  bar_width    = 26,
  label_font_size = "9px",
  title        = "Kjønnsfordeling førstevalgssøkere per utdanningsområde",
  save_path    = "figurer/side_4_kjønn_og_alder_hc/stolpediagram/utdomr_kjonn_stablet.html"
)

utdomr_kjonn_hc



### KJØNNSFORDELING FØRSTEVALGSSØKERE OVER TID ###


# Vertical chart example – region by landsdel
aar_kjonn_data <- dt %>%
  group_by(aar, kjonn) %>%
  summarise(n = n_distinct(regnr), .groups = "drop")

aar_kjonn_hc <- make_stacked_pct_chart(
  data         = aar_kjonn_data,
  category_var = aar,
  stack_var    = kjonn,
  value_var    = n,
  order_by     = NULL,          # years should stay in chronological order
  colors       = c(
    "Mann"   = "#0D47A1",
    "Kvinne" = "#E91E63"
  ),
  orientation  = "vertical",
  chart_height = 620,
  bar_width    = 60,            # columns can be a bit wider than horizontal bars
  label_font_size = "11px",
  title        = "Kjønnsfordeling søkere per år",
  save_path    = file.path("figurer/side_4_kjønn_og_alder_hc/stolpediagram/alle_sokere.html")
)

aar_kjonn_hc



### KJØNSSFORDELING PER UTDANNINGSOMRÅDE - MED NEDTREKKSLISTE ###

### Funksjon med arguemnt for dropdown-liste - FUNKER!!!
source("funksjoner/stablet_stolpe_dropdown_funksjon.R")


dt2 <- dt %>% mutate(aar = as.integer(aar))

# Prepare summarised data for all utd_omr groups at once
utdomr_kjonn_aar <- dt2 %>%
  filter(prioritet == 1) %>%
  group_by(utd_omr, aar, kjonn) %>%
  summarise(n = n_distinct(regnr), .groups = "drop")

# Dropdown
make_stacked_pct_dropdown(
  data           = utdomr_kjonn_aar,
  category_var   = aar,
  stack_var      = kjonn,
  value_var      = n,
  group_var      = utd_omr,
  colors         = c("Mann" = "#0D47A1", "Kvinne" = "#E91E63"),
  orientation    = "vertical",
  chart_height   = 520,
  bar_width      = 60,
  cat_label      = "År",
  stack_label    = "Kjønn",
  dropdown_label = "Utdanningsområde:",
  save_path      = "figurer/side_4_kjønn_og_alder_hc/stolpediagram/kjonn_utd_omr_dropdown.html"
)



### NØDLØSNING FOR DROPDOWN-MENY (Dersom funksjonen ikke fuksjonener ordentlig) ###


## NB: Får ikke tl å lage funksjon med argument for dropdown-meny - må lage disse enkeltvis

# ── 1. Prep data ──────────────────────────────────────────────────────────
dt2 <- dt %>% mutate(aar = as.integer(aar))

utdomr_kjonn_aar <- dt2 %>%
  filter(prioritet == 1) %>%
  group_by(utd_omr, aar, kjonn) %>%
  summarise(n = n_distinct(regnr), .groups = "drop")

group_values <- sort(unique(utdomr_kjonn_aar$utd_omr))

# ── 2. Build charts_data list ─────────────────────────────────────────────
charts_data <- lapply(group_values, function(grp) {
  df <- utdomr_kjonn_aar %>% filter(utd_omr == grp)
  
  years <- sort(unique(df$aar))
  
  kvinner <- sapply(years, function(y) {
    v <- df$n[df$aar == y & df$kjonn == "Kvinne"]
    if (length(v) == 0) 0L else as.integer(v)
  })
  menn <- sapply(years, function(y) {
    v <- df$n[df$aar == y & df$kjonn == "Mann"]
    if (length(v) == 0) 0L else as.integer(v)
  })
  
  # Pre-compute percentage labels for tooltip and data labels
  totals      <- kvinner + menn
  pct_kvinner <- round(100 * kvinner / totals, 1)
  pct_menn    <- round(100 * menn    / totals, 1)
  
  fmt <- function(n, pct) {
    paste0(
      formatC(n, format = "d", big.mark = "\u00a0"),
      " (", formatC(pct, format = "f", digits = 1), "\u00a0%)"
    )
  }
  
  list(
    label        = grp,
    categories   = as.character(years),
    kvinner      = as.integer(kvinner),
    menn         = as.integer(menn),
    lbl_kvinner  = fmt(kvinner, pct_kvinner),  # pre-formatted label strings
    lbl_menn     = fmt(menn,    pct_menn)
  )
})

# ── 3. Dependencies + JSON ────────────────────────────────────────────────
hc_deps     <- Filter(function(d) d$name == "highcharts",
                      htmltools::findDependencies(highchart()))
charts_json <- jsonlite::toJSON(charts_data, auto_unbox = TRUE, null = "null")

# ── 4. Build page ─────────────────────────────────────────────────────────

## ── SETTINGS: adjust these values to change appearance ───────────────────
chart_height    <- 430       # px — height of the chart area
bar_width       <- 60        # px — width of each column
label_font_size <- "11px"    # font size of the in-bar percentage labels
label_threshold <- 8         # minimum % to show a data label
color_kvinne    <- "#E91E63"
color_mann      <- "#0D47A1"
dropdown_label  <- "Utdanningsområde:"
## ─────────────────────────────────────────────────────────────────────────

page <- htmltools::tagList(
  hc_deps[[1]],
  htmltools::tags$style(htmltools::HTML(paste0("
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: Arial, sans-serif; font-size: 13px;
           background: #fff; padding: 10px; }
    #controls { margin-bottom: 12px; }
    #controls label { font-weight: bold; margin-right: 8px; }
    #grp-select { padding: 5px 10px; font-size: 13px; border: 1px solid #ccc;
                  border-radius: 4px; min-width: 220px; cursor: pointer; }
    #chart-container { width: 100%; height: ", chart_height, "px; }
  "))),
  htmltools::tags$div(
    id = "controls",
    htmltools::tags$label(`for` = "grp-select", dropdown_label),
    htmltools::tags$select(id = "grp-select")
  ),
  htmltools::tags$div(id = "chart-container"),
  htmltools::tags$script(htmltools::HTML(paste0('
    const allData       = ', charts_json, ';
    const CHART_HEIGHT  = ', chart_height, ';
    const BAR_WIDTH     = ', bar_width, ';
    const FONT_SIZE     = "', label_font_size, '";
    const THRESHOLD     = ', label_threshold, ';
    const COLOR_KVINNE  = "', color_kvinne, '";
    const COLOR_MANN    = "', color_mann, '";

    const select = document.getElementById("grp-select");
    let chart = null;

    allData.forEach(function(d, i) {
      const opt = document.createElement("option");
      opt.value = i;
      opt.textContent = d.label;
      select.appendChild(opt);
    });

    function renderChart(index) {
      const d = allData[index];
      if (chart) { chart.destroy(); }
      chart = Highcharts.chart("chart-container", {
        chart: {
          type: "column",
          height: CHART_HEIGHT,
          reflow: true
        },
        title: { text: "" },
        xAxis: {
          categories: d.categories,
          title: { text: "" },
          labels: { style: { fontSize: "11px" } }
        },
        yAxis: {
          min: 0, max: 100,
          title: { text: "" },
          labels: { format: "{value}%" },
          reversedStacks: false
        },
        tooltip: {
          useHTML: true,
          formatter: function() {
            var idx = this.point.index;
            var lbl = this.series.name === "Kvinne"
                        ? d.lbl_kvinner[idx]
                        : d.lbl_menn[idx];
            return "År: " + this.x +
                   "<br>Kjønn: " + this.series.name +
                   "<br>Antall / andel: " + lbl;
          }
        },
        plotOptions: {
          series: {
            stacking: "percent",
            borderWidth: 0,
            pointWidth: BAR_WIDTH,    // ← column width
            dataLabels: {
              enabled: true,
              crop: false,
              overflow: "none",
              style: {
                color: "white",
                fontWeight: "normal",
                textOutline: "none",
                fontSize: FONT_SIZE   // ← label font size
              },
              formatter: function() {
                if (this.percentage < THRESHOLD) return null;
                return Highcharts.numberFormat(this.percentage, 1, ",", "\u00a0") + "\u00a0%";
              }
            }
          }
        },
        legend: {
          enabled: true,
          layout: "horizontal",
          align: "center",
          verticalAlign: "bottom",
          itemStyle: { fontSize: "11px", fontWeight: "normal" }
        },
        series: [
          {
            name: "Kvinne",
            data: d.kvinner,
            color: COLOR_KVINNE
          },
          {
            name: "Mann",
            data: d.menn,
            color: COLOR_MANN
          }
        ],
        credits: { enabled: false },
        exporting: { enabled: true }
      });
    }

    select.addEventListener("change", function() {
      renderChart(parseInt(this.value));
    });

    renderChart(0);
  ')))
)

# ── 5. Save ───────────────────────────────────────────────────────────────
out_path <- "figurer/side_4_kjønn_og_alder_hc/stolpediagram/kjonn_utd_omr_dropdown.html"
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
htmltools::save_html(htmltools::browsable(page), file = out_path)
message("Saved to: ", out_path)



