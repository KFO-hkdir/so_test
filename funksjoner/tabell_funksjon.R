
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")
getwd()
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
library(htmlwidgets)
library(plotly)
library(scales)
library(tidyr)
library(htmltools)
library(stringr)
library(sf)
library(csmaps)
library(leaflet)
library(readr)
library(csdata)
library(ggtext)
library(ggmap)
library(scico)
library(fst)
library(RColorBrewer)
library(patchwork)

library(DT)
library(highcharter)
library(htmltools)
library(jsonlite)
library(dplyr)


make_hc_table <- function(
    data,
    display_cols,              # named vector: c("Header Label" = "col_name", ...)
    group_col   = NULL,        # optional string; if NULL, makes a single table
    title       = NULL,
    out_file    = NULL,
    max_width   = 640,
    header_color = "#F58220",
    stripe_color = "#fde9d9",
    sort_by     = NULL,        # optional string; should usually be a numeric helper column
    sort_desc   = TRUE,
    size        = c("normal", "compact", "extra compact"),
    first_col_width = NULL,    # e.g. "220px", "240px", "30%"
    export_xlsx = FALSE,       # adds burger-style XLSX export button
    export_file_stem = "table" # file name stem for downloads
) {
  library(dplyr)
  library(jsonlite)
  library(htmltools)
  
  size <- match.arg(size)
  
  size_settings <- switch(
    size,
    "normal" = list(
      body_font_size   = 13,
      title_font_size  = 16,
      select_font_size = 13,
      select_pad_v     = 6,
      select_pad_h     = 10,
      cell_pad_v       = 6,
      cell_pad_h       = 10,
      menu_button_size = 30,
      menu_font_size   = 13,
      control_gap      = 8
    ),
    "compact" = list(
      body_font_size   = 12,
      title_font_size  = 14,
      select_font_size = 12,
      select_pad_v     = 5,
      select_pad_h     = 8,
      cell_pad_v       = 4,
      cell_pad_h       = 8,
      menu_button_size = 28,
      menu_font_size   = 12,
      control_gap      = 7
    ),
    "extra compact" = list(
      body_font_size   = 11,
      title_font_size  = 13,
      select_font_size = 11,
      select_pad_v     = 4,
      select_pad_h     = 7,
      cell_pad_v       = 3,
      cell_pad_h       = 6,
      menu_button_size = 26,
      menu_font_size   = 11,
      control_gap      = 6
    )
  )
  
  # Validate columns
  src_cols   <- unname(display_cols)
  col_labels <- names(display_cols)
  
  if (is.null(col_labels) || any(col_labels == "")) {
    stop("display_cols must be a named vector, e.g. c('Header' = 'column_name').")
  }
  
  if (!all(src_cols %in% names(data))) {
    missing_cols <- src_cols[!src_cols %in% names(data)]
    stop("These display_cols are not in data: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.null(group_col) && !group_col %in% names(data)) {
    stop("group_col must be the name of a column in data.")
  }
  
  if (!is.null(sort_by) && !sort_by %in% names(data)) {
    stop("sort_by must be the name of a column in data.")
  }
  
  # Optional sorting
  if (!is.null(sort_by)) {
    if (is.null(group_col)) {
      data <- data %>%
        arrange(if (sort_desc) desc(.data[[sort_by]]) else .data[[sort_by]])
    } else {
      data <- data %>%
        group_by(.data[[group_col]]) %>%
        arrange(
          if (sort_desc) desc(.data[[sort_by]]) else .data[[sort_by]],
          .by_group = TRUE
        ) %>%
        ungroup()
    }
  }
  
  # Safe internal JS column ids
  col_ids <- paste0("col_", seq_along(col_labels))
  
  # Optional CSS for first column width
  first_col_css <- ""
  if (!is.null(first_col_width)) {
    first_col_css <- sprintf("
      .hcg-container th:first-child,
      .hcg-container td:first-child {
        width: %s !important;
        min-width: %s !important;
      }
    ", first_col_width, first_col_width)
  }
  
  # Build JSON data
  if (is.null(group_col)) {
    sub <- data %>%
      select(all_of(src_cols)) %>%
      setNames(col_labels)
    
    json_list <- list(
      "default" = lapply(seq_len(nrow(sub)), function(i) as.list(sub[i, ]))
    )
    
    select_control <- NULL
    initial_key    <- "default"
  } else {
    groups <- sort(unique(data[[group_col]]))
    
    json_list <- lapply(setNames(groups, groups), function(g) {
      sub <- data %>%
        filter(.data[[group_col]] == g) %>%
        select(all_of(src_cols)) %>%
        setNames(col_labels)
      
      lapply(seq_len(nrow(sub)), function(i) as.list(sub[i, ]))
    })
    
    opts_html <- paste(
      sprintf('<option value="%s">%s</option>', groups, groups),
      collapse = "\n"
    )
    
    select_control <- tags$select(id = "grp-select", HTML(opts_html))
    initial_key    <- groups[[1]]
  }
  
  json_str <- toJSON(json_list, auto_unbox = TRUE)
  
  # JavaScript column mapping
  js_columns <- paste(
    sprintf("'%s': rows.map(r => r['%s'])", col_ids, col_labels),
    collapse = ",\n            "
  )
  
  js_col_defs <- paste(
    sprintf("{ id: '%s', header: { format: '%s' } }", col_ids, col_labels),
    collapse = ",\n                  "
  )
  
  # XLSX export controls
  export_controls     <- NULL
  export_script_extra <- ""
  
  if (isTRUE(export_xlsx)) {
    export_controls <- tags$div(
      class = "hc-export-wrap",
      tags$button(
        id           = "hc-export-btn",
        class        = "hc-export-btn",
        type         = "button",
        `aria-label` = "Last ned XLSX",
        HTML("&#9776;")
      ),
      tags$div(
        id    = "hc-export-menu",
        class = "hc-export-menu",
        tags$button(
          id    = "hc-export-xlsx",
          class = "hc-export-item",
          type  = "button",
          "Last ned XLSX"
        )
      )
    )
    
    export_script_extra <- sprintf("
      const exportColLabels = %s;

      function sanitizeFilePart(x) {
        return String(x)
          .trim()
          .replace(/[\\\\/:*?\"<>|]+/g, '-')
          .replace(/\\s+/g, '_');
      }

      function fileNameForExport(ext) {
        if (%s) {
          return '%s_' + sanitizeFilePart(document.getElementById('grp-select').value) + '.' + ext;
        }
        return '%s.' + ext;
      }

      function getCurrentVisibleRows() {
        const key = %s ? document.getElementById('grp-select').value : '%s';
        return allData[key] || [];
      }

      function downloadXLSX() {
        const rows = getCurrentVisibleRows();
        const aoa = [
          exportColLabels,
          ...rows.map(r => exportColLabels.map(lbl => r[lbl]))
        ];
        const ws = XLSX.utils.aoa_to_sheet(aoa);
        const wb = XLSX.utils.book_new();
        XLSX.utils.book_append_sheet(wb, ws, 'Tabell');
        XLSX.writeFile(wb, fileNameForExport('xlsx'));
      }

      const exportBtn     = document.getElementById('hc-export-btn');
      const exportMenu    = document.getElementById('hc-export-menu');
      const exportXlsxBtn = document.getElementById('hc-export-xlsx');

      exportBtn.addEventListener('click', function(e) {
        e.stopPropagation();
        exportMenu.classList.toggle('open');
      });

      exportXlsxBtn.addEventListener('click', function() {
        downloadXLSX();
        exportMenu.classList.remove('open');
      });

      document.addEventListener('click', function(e) {
        if (!document.getElementById('hc-export-btn').contains(e.target) &&
            !document.getElementById('hc-export-menu').contains(e.target)) {
          exportMenu.classList.remove('open');
        }
      });
    ",
                                   jsonlite::toJSON(col_labels, auto_unbox = TRUE),
                                   if (is.null(group_col)) "false" else "true",
                                   export_file_stem,
                                   export_file_stem,
                                   if (is.null(group_col)) "false" else "true",
                                   initial_key
    )
  }
  
  # renderTable uses Grid.grid() factory function
  render_fn_filled <- sprintf("
    function renderTable(key) {
      const rows = allData[key] || [];
      const cols = {
        %s
      };

      if (grid) {
        grid.update({
          dataTable: { columns: cols }
        });
      } else {
        grid = Grid.grid('datagrid-container', {
          dataTable: { columns: cols },
          columnDefaults: { resizable: true },
          columns: [
            %s
          ],
          rendering: {
            theme: 'hcg-theme-default hcg-custom-theme'
          }
        });
      }
    }
  ", js_columns, js_col_defs)
  
  js_block <- if (is.null(group_col)) {
    tags$script(HTML(sprintf("
      const allData = %s;
      let grid = null;
      %s
      renderTable('%s');
      %s
    ", json_str, render_fn_filled, initial_key, export_script_extra)))
  } else {
    tags$script(HTML(sprintf("
      const allData = %s;
      let grid = null;
      %s
      const sel = document.getElementById('grp-select');
      renderTable(sel.value);
      sel.addEventListener('change', () => renderTable(sel.value));
      %s
    ", json_str, render_fn_filled, export_script_extra)))
  }
  
  # Build the full HTML page
  page <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/@highcharts/grid-lite/grid-lite.js"),
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/@highcharts/grid-lite/css/grid-lite.css"),
      if (isTRUE(export_xlsx)) tags$script(src = "https://cdn.sheetjs.com/xlsx-0.20.3/package/dist/xlsx.full.min.js"),
      tags$style(HTML(sprintf("
        /* ── Page layout ── */
        body {
          font-family: sans-serif;
          max-width: %dpx;
          margin: 20px auto;
          padding: 0 12px;
          font-size: %dpx;
          box-sizing: border-box;
        }

        h4 {
          font-size: %dpx;
          margin: 0 0 10px 0;
          line-height: 1.25;
        }

        /* ── Controls row (dropdown + export button) ── */
        .hc-table-controls {
          display: flex;
          align-items: center;
          gap: %dpx;
          margin-bottom: 12px;
          width: 100%%;
          box-sizing: border-box;
        }

        .hc-table-select-wrap {
          flex: 1 1 auto;
          min-width: 0;
        }

        .hc-table-select-wrap select {
          font-size: %dpx;
          padding: %dpx %dpx;
          margin-bottom: 0;
          border: 1px solid #ccc;
          border-radius: 4px;
          width: 100%%;
          box-sizing: border-box;
          background: white;
        }

        .hc-table-export-wrap {
          flex: 0 0 auto;
        }

        /* ── Export burger button ── */
        .hc-export-wrap {
          position: relative;
          display: inline-block;
        }

        .hc-export-btn {
          width: %dpx;
          height: %dpx;
          border: 1px solid #ccc;
          border-radius: 4px;
          background: white;
          cursor: pointer;
          font-size: %dpx;
          line-height: 1;
          padding: 0;
        }

        .hc-export-btn:hover { background: #f6f6f6; }

        .hc-export-menu {
          display: none;
          position: absolute;
          right: 0;
          top: calc(100%% + 4px);
          min-width: 140px;
          background: white;
          border: 1px solid #ccc;
          border-radius: 4px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.12);
          z-index: 9999;
          overflow: hidden;
        }

        .hc-export-menu.open { display: block; }

        .hc-export-item {
          display: block;
          width: 100%%;
          border: 0;
          background: white;
          text-align: left;
          padding: 8px 10px;
          cursor: pointer;
          font-size: %dpx;
        }

        .hc-export-item:hover { background: #f6f6f6; }

        /* ── Grid container ── */
        #datagrid-container {
          width: 100%%;
        }

        .hcg-container {
          width: 100%% !important;
        }

        /* ── Grid Lite CSS-variable theme ──
           Variables cascade: table → header / body → rows / cells
           --hcg-font-size        : all cell text size
           --hcg-padding          : all cell padding (overridden per section)
           --hcg-header-background: header bg colour
           --hcg-header-color     : header text colour
           --hcg-header-font-size : header text size  (= body size, bold handles emphasis)
           --hcg-row-odd-background / --hcg-row-even-background : stripe colours
        */
        .hcg-custom-theme {
          --hcg-font-size:              %dpx;
          --hcg-font-family:            sans-serif;
          --hcg-padding:                %dpx %dpx;
          --hcg-header-background:      %s;
          --hcg-header-color:           #ffffff;
          --hcg-header-font-size:       %dpx;
          --hcg-header-font-weight:     bold;
          --hcg-header-padding:         %dpx %dpx;
          --hcg-row-odd-background:     #ffffff;
          --hcg-row-even-background:    %s;
          width: 100%% !important;
        }
        .hcg-custom-theme th {
          padding-left: 14px !important;
          padding-right: 14px !important;
        }

        /* ── Column alignment: right-align all except first ── */
        .hcg-custom-theme th:not(:first-child),
        .hcg-custom-theme td:not(:first-child) {
          text-align: right !important;
          white-space: nowrap;
        }

        .hcg-custom-theme th:first-child,
        .hcg-custom-theme td:first-child {
          text-align: left !important;
          white-space: nowrap;
        }

        /* right-align the sort button/span inside non-first headers */
        .hcg-custom-theme th:not(:first-child) > *,
        .hcg-custom-theme th:not(:first-child) button,
        .hcg-custom-theme th:not(:first-child) span {
          text-align: right !important;
          justify-content: flex-end !important;
        }

        %s
      ",
                              # body / layout
                              max_width,
                              size_settings$body_font_size,
                              size_settings$title_font_size,
                              # controls
                              size_settings$control_gap,
                              size_settings$select_font_size,
                              size_settings$select_pad_v,
                              size_settings$select_pad_h,
                              # export button
                              size_settings$menu_button_size,
                              size_settings$menu_button_size,
                              size_settings$menu_font_size,
                              size_settings$menu_font_size,
                              # --hcg variables
                              size_settings$body_font_size,       # --hcg-font-size
                              size_settings$cell_pad_v,           # --hcg-padding top/bottom
                              size_settings$cell_pad_h,           # --hcg-padding left/right
                              header_color,                       # --hcg-header-background
                              size_settings$body_font_size,       # --hcg-header-font-size
                              size_settings$cell_pad_v,           # --hcg-header-padding top/bottom
                              size_settings$cell_pad_h,           # --hcg-header-padding left/right
                              stripe_color,                       # --hcg-row-even-background
                              # optional first-col width CSS
                              first_col_css
      )))
    ),
    
    tags$body(
      if (!is.null(title)) tags$h4(title),
      
      if (!is.null(select_control) || !is.null(export_controls)) {
        tags$div(
          class = "hc-table-controls",
          if (!is.null(select_control)) {
            tags$div(class = "hc-table-select-wrap", select_control)
          },
          if (!is.null(export_controls)) {
            tags$div(class = "hc-table-export-wrap", export_controls)
          }
        )
      },
      
      tags$div(id = "datagrid-container"),
      
      js_block
    )
  )
  
  if (!is.null(out_file)) {
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    htmltools::save_html(page, out_file)
    message("Saved: ", out_file)
  }
  
  invisible(page)
}

