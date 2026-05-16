build_chart_presentation <- function(
    legend_enabled = TRUE,
    legend_align = "center",
    legend_vertical_align = "bottom",
    legend_layout = "horizontal",
    y_min = 0,
    y_max = NULL,
    y_format = "integer-nb",
    tooltip_mode = "shared-number",
    x_axis_label_rotation = c(-10, -20, -30, -40, -50, -60, -70, -80, -90)) {
  list(
    legend = list(
      enabled = legend_enabled,
      align = legend_align,
      verticalAlign = legend_vertical_align,
      layout = legend_layout
    ),
    yAxis = list(
      min = y_min,
      max = y_max,
      format = y_format
    ),
    xAxis = list(
      labelRotation = x_axis_label_rotation
    ),
    tooltip = list(
      mode = tooltip_mode
    )
  )
}

build_single_chart_spec <- function(
    categories,
    series,
    chart_type = "line",
    title = "",
    subtitle = "",
    presentation = build_chart_presentation()) {
  list(
    version = 1,
    renderer = "highcharts",
    kind = "single",
    chartType = chart_type,
    title = title,
    subtitle = subtitle,
    categories = as.list(categories),
    series = series,
    presentation = presentation
  )
}

build_tabbed_chart_spec <- function(
    control_label,
    items,
    chart_type = "line",
    title = "",
    subtitle = "",
    presentation = build_chart_presentation()) {
  list(
    version = 1,
    renderer = "highcharts",
    kind = "tabbed",
    chartType = chart_type,
    title = title,
    subtitle = subtitle,
    controlLabel = control_label,
    items = items,
    presentation = presentation
  )
}

build_chart_item <- function(label, categories, series) {
  list(
    label = label,
    categories = as.list(categories),
    series = series
  )
}

build_series_spec <- function(name, data) {
  list(
    name = name,
    data = as.list(data)
  )
}

write_chart_json <- function(chart_spec, output_path) {
  jsonlite::write_json(
    chart_spec,
    path = output_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}
