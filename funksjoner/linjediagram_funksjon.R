library(dplyr)
library(highcharter)
library(htmltools)
library(jsonlite)

#' Create a Highcharts line chart — single or with dropdown selection
#'
#' Follows the proven patterns from HIGHCHARTS_BAR_CHART_REFERENCE.R (Pattern 3):
#'   - Flat numeric arrays serialised as named JS variables (dot notation)
#'   - Highcharts dependency injected from local highcharter package (no CDN)
#'   - renderChart(0) called directly (not inside window.onload)
#'   - htmltools::save_html(browsable(...)) for saving (no libdir argument)
#'
#' @param data
#'   A data frame. Must contain one row per combination of
#'   x_var x group_var (if dropdown mode).
#'
#' @param x_var
#'   Unquoted column name for the x-axis (e.g. aar).
#'
#' @param series_vars
#'   A character vector of column names to plot as separate lines,
#'   e.g. c("n_sokere", "antall_studieplasser").
#'
#' @param series_labels
#'   Optional character vector of display names for each series,
#'   same length as series_vars. NULL = uses series_vars as labels.
#'
#' @param group_var
#'   Optional unquoted column name for dropdown grouping (e.g. utd_omr).
#'   NULL = single chart with no dropdown.
#'
#' @param colors
#'   Character vector of hex colours, one per series.
#'   NULL = Highcharts default palette.
#'
#' @param y_min
#'   Numeric. Minimum value for the y-axis. Default 0.
#'
#' @param y_max
#'   Numeric or NULL. Maximum value for the y-axis. NULL = auto.
#'
#' @param y_decimals
#'   Integer. Decimal places in y-axis tick labels and tooltips. Default 0.
#'
#' @param y_decimal_mark
#'   Character. Decimal separator. Default ",".
#'
#' @param y_thousands_sep
#'   Character. Thousands separator. Default non-breaking space "\u00a0".
#'
#' @param x_label
#'   Optional x-axis title string. Default NULL (no title).
#'
#' @param dropdown_label
#'   Label text shown before the dropdown selector. NULL = group column name.
#'
#' @param chart_height
#'   Integer, pixels. Default 420.
#'
#' @param chart_margin
#'   Numeric vector c(top, right, bottom, left). Default c(50, 20, 60, 80).
#'
#' @param line_width
#'   Numeric. Line stroke width. Default 1.5.
#'
#' @param marker_radius
#'   Numeric. Radius of data point markers. Default 4.
#'
#' @param show_legend
#'   Logical. Show the series legend. Default TRUE.
#'
#' @param tooltip_shared
#'   Logical. Show all series in a single shared tooltip. Default TRUE.
#'
#' @param sort_groups
#'   Logical. Sort dropdown groups alphabetically. Default TRUE.
#'   Set FALSE to preserve the order groups appear in the data frame.
#'
#' @param save_path
#'   File path for saving the HTML output. NULL = don't save.
#'
#' @return Invisibly returns the htmltools page object.
#'
#' @examples
#' # Single chart, one series
#' make_line_chart(
#'   data          = utenlandske_aar,
#'   x_var         = aar,
#'   series_vars   = c("sokere"),
#'   series_labels = c("Soekere med utenlandsk utdanning"),
#'   colors        = c(hkdir_farger[1]),
#'   save_path     = "figurer/utenlandske_linje.html"
#' )
#'
#' # Single chart, two series
#' make_line_chart(
#'   data          = dt_all,
#'   x_var         = aar,
#'   series_vars   = c("n_sokere", "antall_studieplasser"),
#'   series_labels = c("Soekere", "Studieplasser"),
#'   colors        = c(hkdir_farger[1], hkdir_farger[2]),
#'   save_path     = "figurer/nasjonalt_linje.html"
#' )
#'
#' # Dropdown chart
#' make_line_chart(
#'   data           = charts_df,
#'   x_var          = aar,
#'   series_vars    = c("n_sokere", "antall_studieplasser"),
#'   series_labels  = c("Soekere", "Studieplasser"),
#'   group_var      = utd_omr,
#'   colors         = c(hkdir_farger[1], hkdir_farger[2]),
#'   dropdown_label = "Utdanningsomraade:",
#'   save_path      = "figurer/utdomr_linje_dropdown.html"
#' )

make_line_chart <- function(
    data,
    x_var,
    series_vars,
    series_labels   = NULL,
    group_var       = NULL,
    colors          = NULL,
    y_min           = 0,
    y_max           = NULL,
    y_decimals      = 0,
    y_decimal_mark  = ",",
    y_thousands_sep = "\u00a0",
    x_label         = NULL,
    dropdown_label  = NULL,
    chart_height    = 420,
    chart_margin    = c(50, 20, 60, 80),
    line_width      = 1.5,
    marker_radius   = 4,
    show_legend     = TRUE,
    tooltip_shared  = TRUE,
    sort_groups     = TRUE,
    title           = NULL,
    save_path       = NULL
) {
  
  # ── Resolve column name strings ─────────────────────────────────────────────
  x_nm <- rlang::as_label(rlang::ensym(x_var))
  
  has_group <- !missing(group_var) && !is.null(substitute(group_var))
  grp_nm    <- if (has_group) rlang::as_label(rlang::ensym(group_var)) else NULL
  
  # ── Validate series_vars ────────────────────────────────────────────────────
  if (!is.character(series_vars) || length(series_vars) < 1) {
    stop("series_vars must be a character vector of column names.")
  }
  missing_cols <- setdiff(series_vars, names(data))
  if (length(missing_cols) > 0) {
    stop("These series_vars are not in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # ── Series display labels ───────────────────────────────────────────────────
  if (is.null(series_labels)) {
    series_labels <- series_vars
  } else if (length(series_labels) != length(series_vars)) {
    stop("series_labels must be the same length as series_vars.")
  }
  
  # ── Dropdown label ──────────────────────────────────────────────────────────
  dropdown_str <- if (!is.null(dropdown_label)) dropdown_label else grp_nm
  
  # ── Colors ──────────────────────────────────────────────────────────────────
  if (is.null(colors)) {
    colors <- c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c",
                "#8085e9", "#f15c80", "#e4d354", "#2b908f",
                "#f45b5b", "#91e8e1")
  }
  colors_used <- rep_len(colors, length(series_vars))
  
  # ── JS scalar helpers ───────────────────────────────────────────────────────
  y_max_js   <- if (is.null(y_max)) "null" else as.character(y_max)
  margin_js  <- paste0("[", paste(chart_margin, collapse = ", "), "]")
  x_title_js <- if (!is.null(x_label)) paste0('"', x_label, '"') else '""'
  title_js   <- if (!is.null(title))   paste0('"', title,   '"') else '""'
  
  # ── Tooltip formatter JS ────────────────────────────────────────────────────
  tooltip_formatter_js <- if (tooltip_shared) {
    paste0(
      'function() {',
      '  var s = "<b>" + this.x + "</b>";',
      '  this.points.forEach(function(p) {',
      '    s += "<br/>" + p.series.name + ": " +',
      '         Highcharts.numberFormat(p.y, ', y_decimals, ', "',
      y_decimal_mark, '", "', y_thousands_sep, '");',
      '  });',
      '  return s;',
      '}'
    )
  } else {
    paste0(
      'function() {',
      '  return "<b>" + this.series.name + "</b><br/>" + this.x + ": " +',
      '         Highcharts.numberFormat(this.y, ', y_decimals, ', "',
      y_decimal_mark, '", "', y_thousands_sep, '");',
      '}'
    )
  }
  
  tooltip_opts_js <- paste0(
    if (tooltip_shared) 'shared: true' else 'shared: false',
    ', formatter: ', tooltip_formatter_js
  )
  
  # ── Highcharts dependency (local package, no CDN) ───────────────────────────
  hc_deps <- Filter(
    function(d) d$name == "highcharts",
    htmltools::findDependencies(highchart())
  )
  
  # ═══════════════════════════════════════════════════════════════════════════
  # BRANCH A: Single chart (no dropdown)
  #
  # Each series value array is serialised as its own named JS constant:
  #   const series_sokere = [3594, 3754, ...];
  # and referenced by variable name in the series config. This avoids any
  # runtime lookup issues and mirrors the working inline script exactly.
  # Colors are embedded as literal strings, not looked up from an array.
  # ═══════════════════════════════════════════════════════════════════════════
  if (!has_group) {
    
    # One JS const per series
    series_consts_js <- paste(
      sapply(series_vars, function(col) {
        vals <- jsonlite::toJSON(as.numeric(data[[col]]), auto_unbox = FALSE)
        paste0("const series_", col, " = ", vals, ";")
      }),
      collapse = "\n"
    )
    
    categories_js <- jsonlite::toJSON(as.character(data[[x_nm]]), auto_unbox = FALSE)
    
    # Series array — variable name reference, color as literal string
    series_array_js <- paste(
      mapply(function(col, lbl, col_color) {
        paste0(
          '{ name: "', lbl, '",',
          ' data: series_', col, ',',
          ' color: "', col_color, '",',
          ' marker: { enabled: true, radius: ', marker_radius, ' },',
          ' dataLabels: { enabled: false },',
          ' label: { enabled: false } }'
        )
      }, series_vars, series_labels, colors_used),
      collapse = ",\n  "
    )
    
    script_js <- paste0(
      series_consts_js, "\n",
      "const categories = ", categories_js, ";\n\n",
      'Highcharts.chart("chart-container", {\n',
      '  chart: { type: "line", reflow: true, margin: ', margin_js, ' },\n',
      '  title: { text: ', title_js, ' },\n',
      '  xAxis: { categories: categories, title: { text: ', x_title_js, ' } },\n',
      '  yAxis: {\n',
      '    min: ', y_min, ', max: ', y_max_js, ',\n',
      '    title: { text: "" },\n',
      '    labels: { formatter: function() {\n',
      '      return Highcharts.numberFormat(this.value, ', y_decimals, ', "',
      y_decimal_mark, '", "', y_thousands_sep, '");\n',
      '    }}\n',
      '  },\n',
      '  legend: {\n',
      '    enabled: ', tolower(as.character(show_legend)), ',\n',
      '    align: "center", verticalAlign: "bottom", layout: "horizontal",y: 10,\n',
      '    itemStyle: { fontSize: "11px", fontWeight: "normal" }\n',
      '  },\n',
      '  tooltip: { ', tooltip_opts_js, ' },\n',
      '  plotOptions: { line: { lineWidth: ', line_width, ' } },\n',
      '  series: [\n  ', series_array_js, '\n  ],\n',
      '  credits: { enabled: false },\n',
      '  exporting: { enabled: true }\n',
      '});'
    )
    
    page <- htmltools::tagList(
      hc_deps[[1]],
      htmltools::tags$style(htmltools::HTML(paste0(
        "* { box-sizing: border-box; margin: 0; padding: 0; }\n",
        "body { font-family: Arial, sans-serif; font-size: 13px; background: #fff; padding: 10px; }\n",
        "#chart-container { width: 100%; height: ", chart_height, "px; }"
      ))),
      htmltools::tags$div(id = "chart-container"),
      htmltools::tags$script(htmltools::HTML(script_js))
    )
    
    if (!is.null(save_path)) {
      dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
      htmltools::save_html(htmltools::browsable(page), file = save_path)
      message("Saved to: ", save_path)
    }
    
    return(invisible(page))
  }
  
  # ═══════════════════════════════════════════════════════════════════════════
  # BRANCH B: Dropdown chart
  #
  # allData is a JSON array; d is always an element of allData inside
  # renderChart(), so d["col"] bracket notation works correctly here.
  # renderChart(0) called directly — no window.onload wrapper.
  # ═══════════════════════════════════════════════════════════════════════════
  
  group_values <- data %>%
    dplyr::pull(dplyr::all_of(grp_nm)) %>%
    as.character() %>%
    unique()
  
  if (sort_groups) group_values <- sort(group_values)
  
  charts_data <- lapply(group_values, function(grp) {
    df <- data %>%
      dplyr::filter(as.character(.data[[grp_nm]]) == grp) %>%
      dplyr::arrange(.data[[x_nm]])
    
    entry <- list(
      label      = grp,
      categories = as.character(df[[x_nm]])
    )
    for (col in series_vars) {
      entry[[col]] <- as.numeric(df[[col]])
    }
    entry
  })
  
  charts_json <- jsonlite::toJSON(charts_data, auto_unbox = TRUE, null = "null")
  
  # Series array for dropdown branch — d["col"] works here because
  # d is always an element of allData, not the top-level parsed object
  series_array_js <- paste(
    mapply(function(col, lbl, col_color) {
      paste0(
        '{ name: "', lbl, '",',
        ' data: d["', col, '"],',
        ' color: "', col_color, '",',
        ' marker: { enabled: true, radius: ', marker_radius, ' },',
        ' dataLabels: { enabled: false },',
        ' label: { enabled: false } }'
      )
    }, series_vars, series_labels, colors_used),
    collapse = ",\n      "
  )
  
  script_js <- paste0(
    'const allData = ', charts_json, ';\n',
    'const select  = document.getElementById("grp-select");\n',
    'let chart = null;\n\n',
    'allData.forEach(function(d, i) {\n',
    '  const opt = document.createElement("option");\n',
    '  opt.value = i;\n',
    '  opt.textContent = d.label;\n',
    '  select.appendChild(opt);\n',
    '});\n\n',
    'function renderChart(index) {\n',
    '  const d = allData[index];\n',
    '  if (!d) return;\n',
    '  if (chart) { chart.destroy(); }\n\n',
    '  chart = Highcharts.chart("chart-container", {\n',
    '    chart: { type: "line", reflow: true, margin: ', margin_js, ' },\n',
    '    title: { text: ', title_js, ' },\n',
    '    xAxis: { categories: d.categories, title: { text: ', x_title_js, ' } },\n',
    '    yAxis: {\n',
    '      min: ', y_min, ', max: ', y_max_js, ',\n',
    '      title: { text: "" },\n',
    '      labels: { formatter: function() {\n',
    '        return Highcharts.numberFormat(this.value, ', y_decimals, ', "',
    y_decimal_mark, '", "', y_thousands_sep, '");\n',
    '      }}\n',
    '    },\n',
    '    legend: {\n',
    '      enabled: ', tolower(as.character(show_legend)), ',\n',
    '      align: "center", verticalAlign: "bottom", layout: "horizontal",\n',
    '      itemStyle: { fontSize: "11px", fontWeight: "normal" }\n',
    '    },\n',
    '    tooltip: { ', tooltip_opts_js, ' },\n',
    '    plotOptions: { line: { lineWidth: ', line_width, ' } },\n',
    '    series: [\n    ', series_array_js, '\n    ],\n',
    '    credits: { enabled: false },\n',
    '    exporting: { enabled: true }\n',
    '  });\n',
    '}\n\n',
    'select.addEventListener("change", function() {\n',
    '  renderChart(parseInt(this.value));\n',
    '});\n\n',
    'renderChart(0);'
  )
  
  page <- htmltools::tagList(
    hc_deps[[1]],
    htmltools::tags$style(htmltools::HTML(paste0(
      "* { box-sizing: border-box; margin: 0; padding: 0; }\n",
      "body { font-family: Arial, sans-serif; font-size: 13px; background: #fff; padding: 10px; }\n",
      "#controls { margin-bottom: 12px; }\n",
      "#controls label { font-weight: bold; margin-right: 8px; }\n",
      "#grp-select { padding: 5px 10px; font-size: 13px; border: 1px solid #ccc;",
      " border-radius: 4px; min-width: 220px; cursor: pointer; }\n",
      "#chart-container { width: 100%; height: ", chart_height, "px; }"
    ))),
    htmltools::tags$div(
      id = "controls",
      htmltools::tags$label(`for` = "grp-select", dropdown_str),
      htmltools::tags$select(id = "grp-select")
    ),
    htmltools::tags$div(id = "chart-container"),
    htmltools::tags$script(htmltools::HTML(script_js))
  )
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    htmltools::save_html(htmltools::browsable(page), file = save_path)
    message("Saved to: ", save_path)
  }
  
  invisible(page)
}
