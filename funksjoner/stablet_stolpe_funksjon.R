library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(highcharter)
library(htmlwidgets)

#' Create a reusable stacked percent bar chart with Highcharts
#'
#' @param data         A data frame (already summarised with counts per group)
#' @param category_var Unquoted column name for the axis categories (e.g. fylke, utd_omr)
#' @param stack_var    Unquoted column name for the stacking groups (e.g. hjem_ut, kjonn)
#' @param value_var    Unquoted column name for the numeric values (e.g. n_applicants, n)
#' @param order_by     One level of stack_var to sort categories by (ascending). NULL = no sorting.
#' @param colors       Named vector of colours keyed to stack_var levels. NULL = Highcharts defaults.
#' @param orientation  "horizontal" (bar) or "vertical" (column). Default "horizontal".
#' @param chart_height Chart height in pixels. Default 700.
#' @param bar_width    pointWidth in pixels. Default 26.
#' @param tooltip_extra_html Optional JS string fragment appended to tooltip. NULL = none.
#' @param title        Chart title string. Default "".
#' @param label_threshold Minimum percentage to show a data label. Default 8.
#' @param save_path    If not NULL, saves self-contained HTML here.
#'
#' @return A Highcharter widget

make_stacked_pct_chart <- function(
    data,
    category_var,
    stack_var,
    value_var,
    order_by           = NULL,
    colors             = NULL,
    orientation        = c("horizontal", "vertical"),
    chart_height       = 700,
    bar_width          = 26,
    label_font_size    = "11px",
    tooltip_extra_html = NULL,
    title              = "",
    label_threshold    = 8,
    cat_label          = NULL,   # custom tooltip label for category axis
    stack_label        = NULL,   # custom tooltip label for stack variable
    save_path          = NULL
) {
  orientation <- match.arg(orientation)
  chart_type  <- if (orientation == "horizontal") "bar" else "column"
  
  cat_col   <- rlang::ensym(category_var)
  stack_col <- rlang::ensym(stack_var)
  val_col   <- rlang::ensym(value_var)
  
  # Fall back to column names if no custom labels supplied
  cat_label_str   <- if (!is.null(cat_label))   cat_label   else rlang::as_label(cat_col)
  stack_label_str <- if (!is.null(stack_label)) stack_label else rlang::as_label(stack_col)
  
  # ── 1. Compute percent + label per category ───────────────────────────────
  df <- data %>%
    rename(
      .cat   = !!cat_col,
      .stack = !!stack_col,
      .val   = !!val_col
    ) %>%
    mutate(
      .cat   = as.character(.cat),
      .stack = as.character(.stack)
    ) %>%
    group_by(.cat) %>%
    mutate(
      .pct   = 100 * .val / sum(.val),
      .label = paste0(
        comma(.val, big.mark = "\u00a0"),
        " (", number(.pct, accuracy = 0.1, decimal.mark = ","), "\u00a0%)"
      )
    ) %>%
    ungroup()
  
  stack_levels <- unique(df$.stack)
  
  # ── 2. Determine category order ──────────────────────────────────────────
  if (!is.null(order_by)) {
    cat_order <- df %>%
      filter(.stack == order_by) %>%
      arrange(.pct) %>%
      pull(.cat)
    missing_cats <- setdiff(unique(df$.cat), cat_order)
    cat_order <- c(cat_order, missing_cats)
  } else {
    cat_order <- unique(df$.cat)
  }
  
  # ── 3. Complete the grid ──────────────────────────────────────────────────
  plot_data <- df %>%
    mutate(
      .cat   = factor(.cat,   levels = cat_order),
      .stack = factor(.stack, levels = stack_levels)
    ) %>%
    complete(
      .cat   = factor(cat_order,    levels = cat_order),
      .stack = factor(stack_levels, levels = stack_levels),
      fill   = list(.val = 0, .pct = 0, .label = "0 (0,0\u00a0%)")
    ) %>%
    arrange(.cat, .stack)
  
  # ── 4. Build per-series point lists ──────────────────────────────────────
  make_series_points <- function(series_name) {
    plot_data %>%
      filter(as.character(.stack) == series_name) %>%
      pmap(function(.cat, .stack, .val, .pct, .label, ...) {
        list(
          name   = as.character(.cat),
          y      = .val,
          custom = list(
            category = as.character(.cat),
            stack    = as.character(.stack),
            label    = .label,
            percent  = .pct
          )
        )
      })
  }
  
  series_list <- map(stack_levels, make_series_points)
  names(series_list) <- stack_levels
  
  # ── 5. Resolve colours ───────────────────────────────────────────────────
  get_color <- function(lvl) {
    if (!is.null(colors) && lvl %in% names(colors)) colors[[lvl]] else NULL
  }
  
  # ── 6. Tooltip JS ────────────────────────────────────────────────────────
  extra <- if (!is.null(tooltip_extra_html)) tooltip_extra_html else ""
  tooltip_js <- JS(paste0(
    "function () {",
    "  return '", cat_label_str, ": ' + this.point.custom.category +",
    "         '<br>", stack_label_str, ": ' + this.point.custom.stack +",
    "         '<br>Antall / andel: ' + this.point.custom.label",
    extra,
    ";",
    "}"
  ))
  
  # ── 7. Data label JS ─────────────────────────────────────────────────────
  label_js <- JS(paste0(
    "function () {",
    "  if (this.percentage < ", label_threshold, ") return null;",
    "  return Highcharts.numberFormat(this.percentage, 1, ',', '\u00a0') + '\u00a0%';",
    "}"
  ))
  
  # ── 8. Assemble chart ────────────────────────────────────────────────────
  hc <- highchart() %>%
    hc_chart(type = chart_type, height = chart_height) %>%
    hc_title(text = title) %>%
    hc_xAxis(
      categories = cat_order,
      title      = list(text = NULL),
      labels     = list(
        rotation = if (orientation == "vertical" && length(cat_order) > 6) -35 else 0,
        style    = list(fontSize = "11px")
      )
    ) %>%
    hc_yAxis(
      min            = 0,
      max            = 100,
      title          = list(text = ""),
      labels         = list(format = "{value}%"),
      reversedStacks = FALSE
    ) %>%
    hc_tooltip(useHTML = TRUE, formatter = tooltip_js) %>%
    hc_plotOptions(
      series = list(
        stacking    = "percent",
        borderWidth = 0,
        dataLabels  = list(
          enabled   = TRUE,
          crop      = FALSE,
          overflow  = "none",
          style     = list(
            color       = "white",
            fontWeight  = "normal",
            textOutline = "none",
            fontSize    = label_font_size
          ),
          formatter = label_js
        )
      ),
      bar    = if (orientation == "horizontal") list(
        pointWidth = bar_width, pointPadding = 0.05, groupPadding = 0.08
      ) else list(),
      column = if (orientation == "vertical") list(
        pointWidth = bar_width, pointPadding = 0.05, groupPadding = 0.08
      ) else list()
    ) %>%
    hc_legend(
      layout        = "horizontal",
      align         = "center",
      verticalAlign = "bottom",
      itemStyle     = list(fontSize = "11px")
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_credits(enabled = FALSE)
  
  for (lvl in stack_levels) {
    hc <- hc %>%
      hc_add_series(name = lvl, data = series_list[[lvl]], color = get_color(lvl))
  }
  
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    htmlwidgets::saveWidget(hc, file = save_path, selfcontained = TRUE)
    message("Saved to: ", save_path)
  }
  
  hc
}