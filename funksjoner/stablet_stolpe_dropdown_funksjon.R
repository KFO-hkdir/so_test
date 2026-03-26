library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(highcharter)
library(htmlwidgets)

make_stacked_pct_dropdown <- function(
    data,
    category_var,
    stack_var,
    value_var,
    group_var,
    order_by           = NULL,
    colors             = NULL,
    orientation        = c("horizontal", "vertical"),
    chart_height       = 520,
    bar_width          = 26,
    label_font_size    = "10px",
    label_threshold    = 8,
    cat_label          = NULL,
    stack_label        = NULL,
    dropdown_label     = NULL,
    save_path          = NULL
) {
  orientation <- match.arg(orientation)
  chart_type  <- if (orientation == "horizontal") "bar" else "column"
  
  cat_nm   <- rlang::as_label(rlang::ensym(category_var))
  stack_nm <- rlang::as_label(rlang::ensym(stack_var))
  val_nm   <- rlang::as_label(rlang::ensym(value_var))
  grp_nm   <- rlang::as_label(rlang::ensym(group_var))
  
  cat_label_str   <- if (!is.null(cat_label))      cat_label      else cat_nm
  stack_label_str <- if (!is.null(stack_label))    stack_label    else stack_nm
  dropdown_str    <- if (!is.null(dropdown_label)) dropdown_label else grp_nm
  
  # All stack levels across the full dataset, in consistent order
  # stack_levels <- data %>%
  #   pull(!!rlang::sym(stack_nm)) %>%
  #   as.character() %>%
  #   unique()
  col <- data[[stack_nm]]
  stack_levels <- if (is.factor(col)) {
    levels(col)[levels(col) %in% unique(as.character(col))]
  } else {
    unique(as.character(col))
  }
  
  group_values <- data %>%
    pull(!!rlang::sym(grp_nm)) %>%
    as.character() %>%
    unique() %>%
    sort()
  
  # ── Format label string ──────────────────────────────────────────────────
  fmt_label <- function(n, pct) {
    paste0(
      formatC(as.integer(n), format = "d", big.mark = "\u00a0"),
      " (",
      formatC(round(pct, 1), format = "f", digits = 1, decimal.mark = ","),
      "\u00a0%)"
    )
  }
  
  # ── Build one list entry per group ───────────────────────────────────────
  # Each entry has: label, categories, and one named flat array per stack
  # level (e.g. "Kvinne", "Mann") plus matching label arrays ("lbl_Kvinne" etc.)
  # This mirrors the working script's d.kvinner / d.lbl_kvinner structure exactly.
  charts_data <- lapply(group_values, function(grp) {
    
    df <- data %>%
      filter(as.character(.data[[grp_nm]]) == grp) %>%
      mutate(
        .cat   = as.character(.data[[cat_nm]]),
        .stack = as.character(.data[[stack_nm]]),
        .val   = as.numeric(.data[[val_nm]])
      )
    
    # Category order
    if (!is.null(order_by)) {
      totals <- df %>%
        group_by(.cat) %>%
        mutate(.total = sum(.val)) %>%
        ungroup() %>%
        filter(.stack == order_by) %>%
        mutate(.pct = 100 * .val / .total) %>%
        arrange(.pct)
      cat_order <- totals$.cat
      cat_order <- c(cat_order, setdiff(unique(df$.cat), cat_order))
    } else {
      cat_order <- df %>% pull(.cat) %>% unique() %>% sort()
    }
    
    # Totals per category for percentage calculation
    cat_totals <- df %>%
      group_by(.cat) %>%
      summarise(.total = sum(.val), .groups = "drop")
    
    # Build one flat integer vector + label vector per stack level,
    # stored as named top-level fields — same shape as working script
    entry <- list(
      label      = grp,
      categories = as.character(cat_order)
    )
    
    for (lvl in stack_levels) {
      vals <- sapply(cat_order, function(cat) {
        v <- df$.val[df$.cat == cat & df$.stack == lvl]
        if (length(v) == 0) 0 else v[1]
      })
      tots <- sapply(cat_order, function(cat) {
        t <- cat_totals$.total[cat_totals$.cat == cat]
        if (length(t) == 0 || t == 0) 1 else t[1]
      })
      pcts <- 100 * vals / tots
      
      # Use the stack level name directly as the field name,
      # e.g. entry[["Kvinne"]] and entry[["lbl_Kvinne"]]
      entry[[lvl]]                    <- as.integer(vals)
      entry[[paste0("lbl_", lvl)]]    <- as.character(mapply(fmt_label, vals, pcts))
    }
    
    entry
  })
  
  # ── Serialise ────────────────────────────────────────────────────────────
  charts_json <- jsonlite::toJSON(charts_data, auto_unbox = TRUE, null = "null")
  
  # ── Build the series JS block dynamically ────────────────────────────────
  # Mirrors: { name: "Kvinne", data: d.kvinner, color: ... }
  # but generated for whatever stack levels exist
  color_list <- colors  # keep reference for closure
  
  series_js <- paste(
    sapply(stack_levels, function(lvl) {
      color_str <- if (!is.null(color_list) && lvl %in% names(color_list)) {
        paste0('"', color_list[[lvl]], '"')
      } else {
        "undefined"
      }
      # Field name in JS: d["Kvinne"], d["lbl_Kvinne"]
      # Using bracket notation to handle spaces or special chars safely
      paste0(
        '{ name: "', lvl, '", ',
        'data: d["', lvl, '"], ',
        'labels: d["lbl_', lvl, '"], ',
        'color: ', color_str, ' }'
      )
    }),
    collapse = ",\n          "
  )
  
  # ── Dependencies ─────────────────────────────────────────────────────────
  hc_deps <- Filter(
    function(d) d$name == "highcharts",
    htmltools::findDependencies(highchart())
  )
  
  # ── Page ─────────────────────────────────────────────────────────────────
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
      htmltools::tags$label(`for` = "grp-select", dropdown_str),
      htmltools::tags$select(id = "grp-select")
    ),
    htmltools::tags$div(id = "chart-container"),
    htmltools::tags$script(htmltools::HTML(paste0('
      const allData       = ', charts_json, ';
      const CHART_TYPE    = "', chart_type, '";
      const CHART_HEIGHT  = ', chart_height, ';
      const BAR_WIDTH     = ', bar_width, ';
      const FONT_SIZE     = "', label_font_size, '";
      const THRESHOLD     = ', label_threshold, ';
      const CAT_LABEL     = "', cat_label_str, '";
      const STACK_LABEL   = "', stack_label_str, '";

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
        if (!d) return;
        if (chart) { chart.destroy(); }

        chart = Highcharts.chart("chart-container", {
          chart: { type: CHART_TYPE, height: CHART_HEIGHT, reflow: true },
          title: { text: "" },
          xAxis: {
            categories: d.categories,
            title: { text: "" },
            labels: {
              rotation: (CHART_TYPE === "column" && d.categories.length > 6) ? -35 : 0,
              style: { fontSize: "11px" }
            }
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
              var lbl = this.series.userOptions.labels[idx];
              return CAT_LABEL + ": " + this.x +
                     "<br>" + STACK_LABEL + ": " + this.series.name +
                     "<br>Antall / andel: " + lbl;
            }
          },
          plotOptions: {
            series: {
              stacking: "percent",
              borderWidth: 0,
              pointWidth: BAR_WIDTH,
              dataLabels: {
                enabled: true,
                crop: false,
                overflow: "none",
                style: {
                  color: "white",
                  fontWeight: "normal",
                  textOutline: "none",
                  fontSize: FONT_SIZE
                },
                formatter: function() {
                  if (this.percentage < THRESHOLD) return null;
                  return Highcharts.numberFormat(this.percentage, 1, ",", "\u00a0") + "\u00a0%";
                }
              }
            }
          },
          legend: {
            layout: "horizontal", align: "center", verticalAlign: "bottom",
            itemStyle: { fontSize: "11px", fontWeight: "normal" }
          },
          series: [
          ', series_js, '
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
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    htmltools::save_html(
      htmltools::browsable(page),
      file = save_path
    )
    message("Saved to: ", save_path)
  }
  
  invisible(page)
}