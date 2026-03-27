library(dplyr)
library(highcharter)
library(htmltools)
library(jsonlite)

#' Create a Highcharts bar/column chart — single or with dropdown selection
#'
#' Follows the proven patterns from HIGHCHARTS_BAR_CHART_REFERENCE.R:
#'   - Flat numeric arrays in JSON (no nested point objects)
#'   - Highcharts dependency injected from local highcharter package (no CDN)
#'   - renderChart(0) called directly (not inside window.onload)
#'   - htmltools::save_html(browsable(...)) for saving (no libdir argument)
#'
#' In dropdown mode, each dropdown entry is a separate metric (value column),
#' and categories are sorted independently per metric — matching the inline
#' script's make_chart_config() pattern exactly.
#'
#' @param data
#'   A data frame with one row per category. Must contain the category column
#'   and all value columns specified in series_configs.
#'
#' @param category_var
#'   Unquoted column name for the bar categories (e.g. utd_omr, fylke).
#'
#' @param series_configs
#'   For a SINGLE chart: a list with one entry:
#'     list(list(
#'       value_col    = "studieplasser",   # column name (string)
#'       series_name  = "Studieplasser",   # legend / tooltip label
#'       decimals     = 0                  # decimal places in labels/tooltip
#'     ))
#'
#'   For a DROPDOWN chart: a list with one entry per dropdown option:
#'     list(
#'       list(value_col = "studieplasser",   series_name = "Studieplasser",              dropdown_label = "Planlagte studieplasser",              decimals = 0),
#'       list(value_col = "forstevalg",      series_name = "Førstevalg",                 dropdown_label = "Førstevalg",                           decimals = 0),
#'       list(value_col = "forste_pr_plass", series_name = "Førstevalg per plass",       dropdown_label = "Førstevalg per planlagte studieplass",  decimals = 1)
#'     )
#'   dropdown_label is only used (and required) when length(series_configs) > 1.
#'
#' @param sort_bars
#'   "desc" (default), "asc", or "none". Each metric sorts its own categories
#'   independently, matching the inline script's per-metric arrange().
#'
#' @param color
#'   Single hex colour string for the bars. Default "#E72F72".
#'
#' @param orientation
#'   "horizontal" (bar, default) or "vertical" (column).
#'
#' @param chart_height
#'   Integer, pixels. Default 520.
#'
#' @param bar_width
#'   Integer, pixels (pointWidth). NULL = Highcharts automatic sizing.
#'
#' @param chart_margin
#'   Numeric vector c(top, right, bottom, left).
#'   Default: c(20, 60, 40, 180) for horizontal, c(20, 20, 60, 60) for vertical.
#'   Supply explicitly to override.
#'
#' @param y_decimal_mark
#'   Decimal separator for y-axis labels and tooltips. Default ",".
#'
#' @param y_thousands_sep
#'   Thousands separator. Default non-breaking space "\u00a0".
#'
#' @param x_axis_font_size
#'   Font size for category axis labels, e.g. "11px". Default "11px".
#'
#' @param dropdown_label
#'   Label text shown before the dropdown selector.
#'   Only used in dropdown mode. Default "Vis:".
#'
#' @param save_path
#'   File path for saving the HTML output. NULL = don't save.
#'
#' @return Invisibly returns the htmltools page object.
#'
#' @examples
#' # ── Single horizontal bar chart ───────────────────────────────────────────
#' make_bar_chart(
#'   data          = utdomr_ferdig,
#'   category_var  = utd_omr,
#'   series_configs = list(
#'     list(value_col = "forstevalg", series_name = "Førstevalg", decimals = 0)
#'   ),
#'   color     = "#E72F72",
#'   save_path = "figurer/forstevalg.html"
#' )
#'
#' # ── Dropdown chart — three metrics, matches inline script exactly ─────────
#' make_bar_chart(
#'   data         = utdomr_ferdig,
#'   category_var = utd_omr,
#'   series_configs = list(
#'     list(value_col = "studieplasser",   series_name = "Studieplasser",        dropdown_label = "Planlagte studieplasser",              decimals = 0),
#'     list(value_col = "forstevalg",      series_name = "Førstevalg",           dropdown_label = "Førstevalg",                           decimals = 0),
#'     list(value_col = "forste_pr_plass", series_name = "Førstevalg per plass", dropdown_label = "Førstevalg per planlagte studieplass",  decimals = 1)
#'   ),
#'   color          = "#E72F72",
#'   dropdown_label = "Vis:",
#'   save_path      = "figurer/utdomr_bars_dropdown.html"
#' )
#'
#' # ── Vertical column chart ─────────────────────────────────────────────────
#' make_bar_chart(
#'   data          = utdomr_ferdig,
#'   category_var  = utd_omr,
#'   series_configs = list(
#'     list(value_col = "forstevalg", series_name = "Førstevalg", decimals = 0)
#'   ),
#'   orientation = "vertical",
#'   bar_width   = 40,
#'   save_path   = "figurer/forstevalg_kolonne.html"
#' )

make_bar_chart <- function(
    data,
    category_var,
    series_configs,
    sort_bars        = c("desc", "asc", "none"),
    color            = "#E72F72",
    orientation      = c("horizontal", "vertical"),
    chart_height     = 520,
    bar_width        = NULL,
    chart_margin     = NULL,
    y_decimal_mark   = ",",
    y_thousands_sep  = "\u00a0",
    x_axis_font_size = "11px",
    dropdown_label   = "Vis:",
    title            = NULL,
    show_legend      = TRUE,
    save_path        = NULL
) {
  
  sort_bars   <- match.arg(sort_bars)
  orientation <- match.arg(orientation)
  chart_type  <- if (orientation == "horizontal") "bar" else "column"
  
  # ── Resolve category column name ────────────────────────────────────────────
  cat_nm <- rlang::as_label(rlang::ensym(category_var))
  
  if (!cat_nm %in% names(data)) {
    stop("category_var '", cat_nm, "' not found in data.")
  }
  
  # ── Validate series_configs ─────────────────────────────────────────────────
  if (!is.list(series_configs) || length(series_configs) == 0) {
    stop("series_configs must be a non-empty list.")
  }
  
  is_dropdown <- length(series_configs) > 1
  
  for (i in seq_along(series_configs)) {
    cfg <- series_configs[[i]]
    if (is.null(cfg$value_col))   stop("series_configs[[", i, "]] is missing 'value_col'.")
    if (is.null(cfg$series_name)) stop("series_configs[[", i, "]] is missing 'series_name'.")
    if (!cfg$value_col %in% names(data)) {
      stop("value_col '", cfg$value_col, "' not found in data.")
    }
    if (is_dropdown && is.null(cfg$dropdown_label)) {
      stop("series_configs[[", i, "]] is missing 'dropdown_label' (required in dropdown mode).")
    }
    if (is.null(cfg$decimals)) series_configs[[i]]$decimals <- 0
  }
  
  # ── Default chart margin ─────────────────────────────────────────────────────
  if (is.null(chart_margin)) {
    chart_margin <- if (orientation == "horizontal") c(20, 60, 40, 180) else c(20, 20, 60, 60)
  }
  margin_js  <- paste0("[", paste(chart_margin, collapse = ", "), "]")
  title_js   <- if (!is.null(title)) paste0('"', title, '"') else '""'
  
  # ── pointWidth JS ───────────────────────────────────────────────────────────
  point_width_js <- if (!is.null(bar_width)) {
    paste0("pointWidth: ", as.integer(bar_width), ",")
  } else {
    ""
  }
  
  # ── Highcharts dependency (local package — no CDN) ──────────────────────────
  hc_deps <- Filter(
    function(d) d$name == "highcharts",
    htmltools::findDependencies(highchart())
  )
  
  # ── Build one chart-data entry per series config ────────────────────────────
  # Each entry sorts categories independently on its own value column,
  # exactly matching the inline script's make_chart_config() pattern.
  build_entry <- function(cfg) {
    
    df <- data %>%
      dplyr::mutate(.cat = as.character(.data[[cat_nm]]),
                    .val = .data[[cfg$value_col]])
    
    df <- switch(
      sort_bars,
      "desc" = dplyr::arrange(df, dplyr::desc(.val)),
      "asc"  = dplyr::arrange(df, .val),
      "none" = df
    )
    
    entry <- list(
      categories = df$.cat,
      values     = if (cfg$decimals == 0) as.integer(df$.val) else as.numeric(df$.val),
      seriesName = cfg$series_name,
      decimals   = cfg$decimals
    )
    
    if (is_dropdown) entry$label <- cfg$dropdown_label
    
    entry
  }
  
  charts_data <- lapply(series_configs, build_entry)
  charts_json <- jsonlite::toJSON(charts_data, auto_unbox = TRUE, null = "null")
  
  # ── Shared chart JS (used in both single and dropdown renderChart()) ─────────
  # d is the current data entry; index is 0-based in JS
  render_chart_js <- paste0('
    function renderChart(index) {
      const d = allData[index];
      if (!d) return;
      if (chart) { chart.destroy(); }

      chart = Highcharts.chart("chart-container", {
        chart: {
          type: "', chart_type, '",
          reflow: true,
          margin: ', margin_js, '
        },
        title:    { text: ', title_js, ' },
        subtitle: { text: "" },
        xAxis: {
          categories: d.categories,
          title: { text: "" },
          labels: { style: { fontSize: "', x_axis_font_size, '" } }
        },
        yAxis: {
          min: 0,
          title: { text: "" },
          labels: {
            formatter: function() {
              return Highcharts.numberFormat(this.value, d.decimals, "', y_decimal_mark, '", "', y_thousands_sep, '");
            }
          }
        },
        legend: {
          enabled: ', tolower(as.character(show_legend)), ',
          align: "center",
          verticalAlign: "bottom",
          layout: "horizontal",
          y: 20,
          itemStyle: { fontSize: "11px", fontWeight: "normal" }
        },
        tooltip: {
          formatter: function() {
            return "<b>" + this.x + "</b><br/>" + d.seriesName + ": " +
                   Highcharts.numberFormat(this.y, d.decimals, "', y_decimal_mark, '", "', y_thousands_sep, '");
          }
        },
        plotOptions: {
          ', chart_type, ': {
            ', point_width_js, '
            pointPadding: 0.01,
            colorByPoint: false,
            dataLabels: { enabled: false },
            label:       { enabled: false }
          }
        },
        series: [{
          name:  d.seriesName,
          data:  d.values,
          color: COLOR
        }],
        credits: { enabled: false },
        exporting: {
          enabled: true,
          csv: {
            columnHeaderFormatter: function(item, key) {
              if (!item || item.isXAxis) { return "', cat_nm, '"; }
              return item.name;
            }
          }
        }
      });
    }
  ')
  
  # ═══════════════════════════════════════════════════════════════════════════
  # BRANCH A — Single chart
  # ═══════════════════════════════════════════════════════════════════════════
  if (!is_dropdown) {
    
    page <- htmltools::tagList(
      hc_deps[[1]],
      htmltools::tags$style(htmltools::HTML(paste0("
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body { font-family: Arial, sans-serif; font-size: 13px;
               background: #fff; padding: 10px; }
        #chart-container { width: 100%; height: ", chart_height, "px; }
      "))),
      htmltools::tags$div(id = "chart-container"),
      htmltools::tags$script(htmltools::HTML(paste0('
        const COLOR   = "', color, '";
        const allData = ', charts_json, ';
        let chart = null;

        ', render_chart_js, '

        renderChart(0);
      ')))
    )
    
    if (!is.null(save_path)) {
      dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
      htmltools::save_html(htmltools::browsable(page), file = save_path)
      message("Saved to: ", save_path)
    }
    
    return(invisible(page))
  }
  
  # ═══════════════════════════════════════════════════════════════════════════
  # BRANCH B — Dropdown chart
  # ═══════════════════════════════════════════════════════════════════════════
  
  page <- htmltools::tagList(
    hc_deps[[1]],
    htmltools::tags$style(htmltools::HTML(paste0("
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body { font-family: Arial, sans-serif; font-size: 13px;
             background: #fff; padding: 10px; }
      #controls { margin-bottom: 12px; }
      #controls label { font-weight: bold; margin-right: 8px; }
      #chart-select {
        padding: 5px 10px;
        font-size: 13px;
        border: 1px solid #ccc;
        border-radius: 4px;
        min-width: 260px;
        cursor: pointer;
      }
      #chart-container { width: 100%; height: ", chart_height, "px; }
    "))),
    htmltools::tags$div(
      id = "controls",
      htmltools::tags$label(`for` = "chart-select", dropdown_label),
      htmltools::tags$select(id = "chart-select")
    ),
    htmltools::tags$div(id = "chart-container"),
    htmltools::tags$script(htmltools::HTML(paste0('
      const COLOR   = "', color, '";
      const allData = ', charts_json, ';

      const select = document.getElementById("chart-select");
      let chart = null;

      allData.forEach(function(d, i) {
        const opt = document.createElement("option");
        opt.value = i;
        opt.textContent = d.label;
        select.appendChild(opt);
      });

      ', render_chart_js, '

      select.addEventListener("change", function() {
        renderChart(parseInt(this.value));
      });

      renderChart(0);
    ')))
  )
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    htmltools::save_html(htmltools::browsable(page), file = save_path)
    message("Saved to: ", save_path)
  }
  
  invisible(page)
}
