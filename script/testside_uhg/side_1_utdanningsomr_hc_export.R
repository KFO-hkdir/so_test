source("script/helpers/chart_json_export.R")

`%||%` <- function(left, right) {
  if (is.null(left) || length(left) == 0) {
    return(right)
  }

  left
}

extract_assigned_array <- function(content, variable_name) {
  pattern <- sprintf("(?:const|let|var)\\s+%s\\s*=\\s*|%s\\s*=\\s*", variable_name, variable_name)
  match <- regexpr(pattern, content, perl = TRUE)

  if (match[1] == -1) {
    return(NULL)
  }

  search_start <- match[1] + attr(match, "match.length")
  array_start_match <- regexpr("\\[", substring(content, search_start), perl = TRUE)

  if (array_start_match[1] == -1) {
    return(NULL)
  }

  array_start <- search_start + array_start_match[1] - 1
  depth <- 0
  in_string <- FALSE
  current_quote <- ""

  for (index in seq(array_start, nchar(content))) {
    character <- substr(content, index, index)
    previous_character <- if (index > 1) substr(content, index - 1, index - 1) else ""

    if (
      character %in% c("\"", "'") &&
      previous_character != "\\" &&
      (!in_string || current_quote == character)
    ) {
      in_string <- !in_string
      current_quote <- if (in_string) character else ""
      next
    }

    if (in_string) {
      next
    }

    if (character == "[") {
      depth <- depth + 1
    }

    if (character == "]") {
      depth <- depth - 1

      if (depth == 0) {
        return(substr(content, array_start, index))
      }
    }
  }

  NULL
}

extract_html_label <- function(content) {
  match <- regexec("<label[^>]*>([[:space:][:print:]]*?)</label>", content, perl = TRUE)
  value <- regmatches(content, match)[[1]]

  if (length(value) < 2) {
    return(NULL)
  }

  trimws(gsub("<[^>]+>", "", value[2]))
}

build_line_chart_spec_from_all_data <- function(all_data, control_label) {
  items <- lapply(all_data, function(item) {
    build_chart_item(
      label = item$label,
      categories = item$categories,
      series = list(
        build_series_spec("Søkere", item$n_sokere),
        build_series_spec("Studieplasser", item$antall_studieplasser)
      )
    )
  })

  if (length(items) == 1 && is.null(control_label)) {
    single_item <- items[[1]]

    return(build_single_chart_spec(
      categories = single_item$categories,
      series = single_item$series,
      chart_type = "line",
      presentation = build_chart_presentation(
        y_min = 0,
        tooltip_mode = "shared-number"
      )
    ))
  }

  build_tabbed_chart_spec(
    control_label = control_label %||% "Velg verdi",
    items = items,
    chart_type = "line",
    presentation = build_chart_presentation(
      y_min = 0,
      tooltip_mode = "shared-number"
    )
  )
}

build_bar_chart_spec_from_all_data <- function(all_data, control_label) {
  items <- lapply(all_data, function(item) {
    build_chart_item(
      label = item$label %||% item$seriesName,
      categories = item$categories,
      series = list(build_series_spec(item$seriesName, item$values))
    )
  })

  decimals <- max(unlist(lapply(all_data, function(item) item$decimals %||% 0)))
  y_format <- if (decimals > 0) "decimal" else "integer-nb"

  if (length(items) == 1 && is.null(control_label)) {
    single_item <- items[[1]]

    return(build_single_chart_spec(
      categories = single_item$categories,
      series = single_item$series,
      chart_type = "bar",
      presentation = build_chart_presentation(
        y_min = 0,
        y_format = y_format,
        tooltip_mode = "shared-number"
      )
    ))
  }

  build_tabbed_chart_spec(
    control_label = control_label %||% "Velg verdi",
    items = items,
    chart_type = "bar",
    presentation = build_chart_presentation(
      y_min = 0,
      y_format = y_format,
      tooltip_mode = "shared-number"
    )
  )
}

build_single_line_chart_spec <- function(content) {
  categories_raw <- extract_assigned_array(content, "categories")
  sokere_raw <- extract_assigned_array(content, "series_n_sokere")
  plasser_raw <- extract_assigned_array(content, "series_antall_studieplasser")

  if (is.null(categories_raw) || is.null(sokere_raw) || is.null(plasser_raw)) {
    stop("Could not extract line chart arrays from html")
  }

  build_single_chart_spec(
    categories = jsonlite::fromJSON(categories_raw),
    series = list(
      build_series_spec("Søkere", jsonlite::fromJSON(sokere_raw)),
      build_series_spec("Studieplasser", jsonlite::fromJSON(plasser_raw))
    ),
    chart_type = "line",
    presentation = build_chart_presentation(
      y_min = 0,
      tooltip_mode = "shared-number"
    )
  )
}

build_chart_spec_from_html <- function(content) {
  control_label <- extract_html_label(content)
  all_data_raw <- extract_assigned_array(content, "allData")

  if (!is.null(all_data_raw)) {
    all_data <- jsonlite::fromJSON(all_data_raw, simplifyVector = FALSE)

    if (length(all_data) == 0) {
      stop("allData was present but empty")
    }

    first_item <- all_data[[1]]

    if (!is.null(first_item$n_sokere) && !is.null(first_item$antall_studieplasser)) {
      return(build_line_chart_spec_from_all_data(all_data, control_label))
    }

    if (!is.null(first_item$values) && !is.null(first_item$seriesName)) {
      return(build_bar_chart_spec_from_all_data(all_data, control_label))
    }

    stop("Unsupported allData structure in html")
  }

  build_single_line_chart_spec(content)
}

export_chart_json_from_html <- function(html_path) {
  content <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  chart_spec <- build_chart_spec_from_html(content)
  output_path <- sub("\\.html$", ".chart.json", html_path)

  write_chart_json(chart_spec, output_path)

  output_path
}

html_paths <- list.files(
  path = "figurer/side_1_utdanningsomr_hc",
  pattern = "\\.html$",
  recursive = TRUE,
  full.names = TRUE
)

generated_json <- vapply(html_paths, export_chart_json_from_html, character(1))
print(generated_json)
