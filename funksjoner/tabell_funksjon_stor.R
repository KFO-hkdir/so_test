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


make_hc_table_l <- function(
    data,
    display_cols,              # named vector: c("Header Label" = "col_name", ...)
    group_col    = NULL,       # optional string; if NULL, makes a single table
    title        = NULL,
    out_file     = NULL,
    max_width    = 960,        # content area width in px (body max-width)
    header_color = "#F58220",
    stripe_color = "#fde9d9",
    sort_by      = NULL,
    sort_desc    = TRUE,
    size         = c("normal", "compact", "extra compact"),
    export_xlsx      = FALSE,
    export_file_stem = "table",
    filter_cols   = FALSE,     # TRUE: add per-column search inputs above table
    scroll_height = "600px",   # height of scrollable table body; NULL = no scroll
    # col_shares: numeric vector of percentage widths, one per display column.
    # Must sum to 100. Shares are converted to pixels from max_width so the
    # table fills the content area exactly — no gaps and no independent scrolling.
    # Example for four columns: c(40, 20, 20, 20)
    # NULL = let Grid Lite size columns automatically.
    col_shares    = NULL
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
  
  # ── Validate columns ──────────────────────────────────────────────────────
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
  
  # ── Validate col_shares and derive pixel widths ───────────────────────────
  # body padding is 0 16px on each side = 32px total; subtract so column pixels
  # sum to the actual rendered content width.
  # An additional 2px fudge is subtracted to absorb Grid Lite's internal border
  # and any sub-pixel rounding, which would otherwise cause a 1-2px horizontal
  # scrollbar to appear even when the content fits.
  body_padding_total <- 32L
  grid_border_fudge  <- 2L
  content_width      <- max_width - body_padding_total - grid_border_fudge
  
  col_px <- NULL  # stays NULL when col_shares not supplied
  
  if (!is.null(col_shares)) {
    if (!is.numeric(col_shares) || any(col_shares <= 0)) {
      stop("col_shares must be a numeric vector of positive values.")
    }
    if (length(col_shares) != length(display_cols)) {
      stop(sprintf(
        "col_shares has %d element(s) but display_cols has %d column(s).",
        length(col_shares), length(display_cols)
      ))
    }
    if (abs(sum(col_shares) - 100) > 0.01) {
      stop(sprintf("col_shares must sum to 100 (got %.4g).", sum(col_shares)))
    }
    # floor each column then give any rounding remainder to the last column
    col_px <- floor(content_width * col_shares / 100)
    col_px[length(col_px)] <- content_width - sum(col_px[-length(col_px)])
    col_px <- as.integer(col_px)
  }
  
  # ── Optional sorting ──────────────────────────────────────────────────────
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
  
  col_ids <- paste0("col_", seq_along(col_labels))
  
  # \n in header names -> <br> for Grid Lite; plain version for filter / XLSX
  col_labels_html  <- gsub("\n", "<br>", col_labels, fixed = TRUE)
  col_labels_plain <- gsub("\n", " ",    col_labels, fixed = TRUE)
  
  # ── Build JSON data ───────────────────────────────────────────────────────
  if (is.null(group_col)) {
    sub <- data %>%
      select(all_of(src_cols)) %>%
      setNames(col_labels_plain)
    
    json_list      <- list("default" = lapply(seq_len(nrow(sub)), function(i) as.list(sub[i, ])))
    select_control <- NULL
    initial_key    <- "default"
  } else {
    groups <- sort(unique(data[[group_col]]))
    
    json_list <- lapply(setNames(groups, groups), function(g) {
      sub <- data %>%
        filter(.data[[group_col]] == g) %>%
        select(all_of(src_cols)) %>%
        setNames(col_labels_plain)
      lapply(seq_len(nrow(sub)), function(i) as.list(sub[i, ]))
    })
    
    opts_html      <- paste(sprintf('<option value="%s">%s</option>', groups, groups), collapse = "\n")
    select_control <- tags$select(id = "grp-select", HTML(opts_html))
    initial_key    <- groups[[1]]
  }
  
  json_str <- toJSON(json_list, auto_unbox = TRUE)
  
  # ── JavaScript column mapping ─────────────────────────────────────────────
  js_columns <- paste(
    sprintf("'%s': filteredRows.map(r => r['%s'])", col_ids, col_labels_plain),
    collapse = ",\n            "
  )
  
  # Column definitions — include pixel width when col_shares was supplied
  if (!is.null(col_px)) {
    js_col_defs <- paste(
      mapply(function(id, lbl_html, w) {
        sprintf("{ id: '%s', header: { format: '%s' }, width: %d }", id, lbl_html, w)
      }, col_ids, col_labels_html, col_px),
      collapse = ",\n                  "
    )
  } else {
    js_col_defs <- paste(
      sprintf("{ id: '%s', header: { format: '%s' } }", col_ids, col_labels_html),
      collapse = ",\n                  "
    )
  }
  
  # ── Per-column CSS (only when col_shares is supplied) ─────────────────────
  # Both table cells (nth-child) and filter inputs (nth-child) are pinned to
  # the same pixel values so they stay visually aligned at all times.
  col_cell_css   <- ""
  col_filter_css <- ""
  
  if (!is.null(col_px)) {
    col_cell_css <- paste(
      sapply(seq_along(col_px), function(i) {
        sprintf(
          "  .hcg-container th:nth-child(%d),\n  .hcg-container td:nth-child(%d) { width: %dpx !important; min-width: %dpx !important; max-width: %dpx !important; }",
          i, i, col_px[i], col_px[i], col_px[i]
        )
      }),
      collapse = "\n"
    )
    col_filter_css <- paste(
      sapply(seq_along(col_px), function(i) {
        sprintf(
          "  .hc-filter-row input.hc-col-filter:nth-child(%d) { flex: 0 0 %dpx !important; width: %dpx !important; min-width: %dpx !important; max-width: %dpx !important; }",
          i, col_px[i], col_px[i], col_px[i], col_px[i]
        )
      }),
      collapse = "\n"
    )
  }
  
  # ── Filter row HTML & JS ──────────────────────────────────────────────────
  filter_html_block <- NULL
  filter_js_block   <- ""
  
  if (isTRUE(filter_cols)) {
    numeric_labels <- col_labels_plain[sapply(src_cols, function(cn) is.numeric(data[[cn]]))]
    
    input_items <- vapply(col_labels_plain, function(lbl) {
      is_num <- lbl %in% numeric_labels
      ph     <- if (is_num) paste0("Filter ", lbl, "\u2026") else paste0("S\u00f8k ", lbl, "\u2026")
      type_  <- if (is_num) "number" else "text"
      sprintf(
        '<input type="%s" class="hc-col-filter" data-label="%s" placeholder="%s" step="any" />',
        type_, gsub('"', '&quot;', lbl), ph
      )
    }, character(1))
    
    filter_html_block <- tags$div(
      class = "hc-filter-row",
      lapply(input_items, HTML)
    )
    
    filter_js_block <- sprintf(
      '
      document.querySelectorAll(".hc-col-filter").forEach(function(inp) { inp.value = ""; });

      const colLabels = %s;

      function getFilterValues() {
        const vals = {};
        document.querySelectorAll(".hc-col-filter").forEach(function(inp) {
          vals[inp.dataset.label] = inp.value.trim().toLowerCase();
        });
        return vals;
      }

      function applyFilters(rows) {
        const filters = getFilterValues();
        return rows.filter(function(row) {
          return colLabels.every(function(lbl) {
            const fv   = filters[lbl];
            if (!fv) return true;
            const cell = String(row[lbl] == null ? "" : row[lbl]).toLowerCase();
            return cell.includes(fv);
          });
        });
      }

      document.querySelectorAll(".hc-col-filter").forEach(function(inp) {
        inp.addEventListener("input", function() {
          const key = %s;
          renderTable(key);
        });
      });
      ',
      toJSON(col_labels_plain, auto_unbox = TRUE),
      if (is.null(group_col)) '"default"' else 'document.getElementById("grp-select").value'
    )
  }
  
  # ── renderTable ───────────────────────────────────────────────────────────
  render_fn_filled <- sprintf(
    '
    function renderTable(key) {
      const rawRows      = allData[key] || [];
      const filteredRows = (typeof applyFilters === "function") ? applyFilters(rawRows) : rawRows;
      const cols = { %s };

      if (grid) {
        grid.update({ dataTable: { columns: cols } });
      } else {
        grid = Grid.grid("datagrid-container", {
          dataTable: { columns: cols },
          columnDefaults: { resizable: true },
          columns: [ %s ],
          rendering: { theme: "hcg-theme-default hcg-custom-theme" }
        });
      }
    }
    ',
    js_columns, js_col_defs
  )
  
  # ── XLSX export ────────────────────────────────────────────────────────────
  export_controls     <- NULL
  export_script_extra <- ""
  
  if (isTRUE(export_xlsx)) {
    export_controls <- tags$div(
      class = "hc-export-wrap",
      tags$button(
        id = "hc-export-btn", class = "hc-export-btn", type = "button",
        `aria-label` = "Last ned XLSX", HTML("&#9776;")
      ),
      tags$div(
        id = "hc-export-menu", class = "hc-export-menu",
        tags$button(id = "hc-export-xlsx", class = "hc-export-item",
                    type = "button", "Last ned XLSX")
      )
    )
    
    export_script_extra <- sprintf(
      '
      const exportColLabels = %s;

      function sanitizeFilePart(x) {
        return String(x).trim().replace(/[\\\\/:*?"<>|]+/g, "-").replace(/\\s+/g, "_");
      }
      function fileNameForExport(ext) {
        if (%s) return "%s_" + sanitizeFilePart(document.getElementById("grp-select").value) + "." + ext;
        return "%s." + ext;
      }
      function getCurrentVisibleRows() {
        const key     = %s ? document.getElementById("grp-select").value : "%s";
        const rawRows = allData[key] || [];
        return (typeof applyFilters === "function") ? applyFilters(rawRows) : rawRows;
      }
      function downloadXLSX() {
        const rows = getCurrentVisibleRows();
        const aoa  = [ exportColLabels, ...rows.map(r => exportColLabels.map(lbl => r[lbl])) ];
        const ws   = XLSX.utils.aoa_to_sheet(aoa);
        const wb   = XLSX.utils.book_new();
        XLSX.utils.book_append_sheet(wb, ws, "Tabell");
        XLSX.writeFile(wb, fileNameForExport("xlsx"));
      }

      const exportBtn     = document.getElementById("hc-export-btn");
      const exportMenu    = document.getElementById("hc-export-menu");
      const exportXlsxBtn = document.getElementById("hc-export-xlsx");

      exportBtn.addEventListener("click", function(e) { e.stopPropagation(); exportMenu.classList.toggle("open"); });
      exportXlsxBtn.addEventListener("click", function() { downloadXLSX(); exportMenu.classList.remove("open"); });
      document.addEventListener("click", function(e) {
        if (!document.getElementById("hc-export-btn").contains(e.target) &&
            !document.getElementById("hc-export-menu").contains(e.target)) {
          exportMenu.classList.remove("open");
        }
      });
      ',
      toJSON(col_labels_plain, auto_unbox = TRUE),
      if (is.null(group_col)) "false" else "true",
      export_file_stem, export_file_stem,
      if (is.null(group_col)) "false" else "true",
      initial_key
    )
  }
  
  # ── JS block ───────────────────────────────────────────────────────────────
  js_block <- if (is.null(group_col)) {
    tags$script(HTML(sprintf(
      'const allData = %s; let grid = null; %s %s renderTable("default"); %s',
      json_str, render_fn_filled, filter_js_block, export_script_extra
    )))
  } else {
    tags$script(HTML(sprintf(
      'const allData = %s; let grid = null; %s %s
       const sel = document.getElementById("grp-select");
       renderTable(sel.value);
       sel.addEventListener("change", () => renderTable(sel.value));
       %s',
      json_str, render_fn_filled, filter_js_block, export_script_extra
    )))
  }
  
  # ── Full HTML page ─────────────────────────────────────────────────────────
  page <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/@highcharts/grid-lite/grid-lite.js"),
      tags$link(rel = "stylesheet",
                href = "https://cdn.jsdelivr.net/npm/@highcharts/grid-lite/css/grid-lite.css"),
      if (isTRUE(export_xlsx))
        tags$script(src = "https://cdn.sheetjs.com/xlsx-0.20.3/package/dist/xlsx.full.min.js"),
      tags$style(HTML(sprintf(
        '
        html { height: 100%%; overflow: hidden; margin: 0; padding: 0; box-sizing: border-box; }
        body {
          height: 100%%;
          overflow: hidden;
          display: flex;
          flex-direction: column;
          font-family: sans-serif;
          max-width: %dpx;
          margin: 0 auto;
          padding: 8px 16px;
          font-size: %dpx;
          box-sizing: border-box;
        }
        h4 { font-size: %dpx; margin: 0 0 12px 0; line-height: 1.25; }

        .hc-table-controls {
          display: flex; align-items: center; gap: %dpx;
          margin-bottom: 10px; width: 100%%; box-sizing: border-box;
        }
        .hc-table-select-wrap { flex: 1 1 auto; min-width: 0; }
        .hc-table-select-wrap select {
          font-size: %dpx; padding: %dpx %dpx;
          border: 1px solid #ccc; border-radius: 4px;
          width: 100%%; box-sizing: border-box; background: white;
        }
        .hc-table-export-wrap { flex: 0 0 auto;   margin-left: auto;}

        /* Filter row — no gap, no scroll; inputs are flush with column edges */
        .hc-filter-row {
          display: flex; gap: 0; margin-bottom: 8px;
          flex-wrap: nowrap; overflow: hidden;
        }
        .hc-filter-row input.hc-col-filter {
          flex: 1 1 0; min-width: 0;
          font-size: %dpx; padding: 4px 6px;
          border: 1px solid #ccc; border-right-width: 0;
          box-sizing: border-box; background: #fafafa;
        }
        .hc-filter-row input.hc-col-filter:last-child { border-right-width: 1px; }
        .hc-filter-row input.hc-col-filter:focus { outline: none; border-color: %s; background: #fff; }

        /* Per-column filter widths */
%s

        .hc-export-wrap { position: relative; display: inline-block; }
        .hc-export-btn {
          width: %dpx; height: %dpx; border: 1px solid #ccc; border-radius: 4px;
          background: white; cursor: pointer; font-size: %dpx; line-height: 1; padding: 0;
        }
        .hc-export-btn:hover { background: #f6f6f6; }
        .hc-export-menu {
          display: none; position: absolute; right: 0; top: calc(100%% + 4px);
          min-width: 140px; background: white; border: 1px solid #ccc;
          border-radius: 4px; box-shadow: 0 2px 8px rgba(0,0,0,0.12); z-index: 9999; overflow: hidden;
        }
        .hc-export-menu.open { display: block; }
        .hc-export-item {
          display: block; width: 100%%; border: 0; background: white;
          text-align: left; padding: 8px 10px; cursor: pointer; font-size: %dpx;
        }
        .hc-export-item:hover { background: #f6f6f6; }

        #datagrid-container { width: 100%%; flex: 1 1 0; min-height: 0; overflow: hidden; }
        .hcg-container { width: 100%% !important; height: 100%% !important; }

        .hcg-custom-theme {
          --hcg-font-size:           %dpx;
          --hcg-font-family:         sans-serif;
          --hcg-padding:             %dpx %dpx;
          --hcg-header-background:   %s;
          --hcg-header-color:        #ffffff;
          --hcg-header-font-size:    %dpx;
          --hcg-header-font-weight:  bold;
          --hcg-header-padding:      %dpx %dpx;
          --hcg-row-odd-background:  #ffffff;
          --hcg-row-even-background: %s;
          width: 100%% !important;
        }
        .hcg-custom-theme th { padding-left: 14px !important; padding-right: 14px !important; }
        .hcg-custom-theme th:not(:first-child),
        .hcg-custom-theme td:not(:first-child) { text-align: right !important; white-space: nowrap; }
        .hcg-custom-theme th:first-child,
        .hcg-custom-theme td:first-child { text-align: left !important; white-space: nowrap; }
        .hcg-custom-theme th:not(:first-child) > *,
        .hcg-custom-theme th:not(:first-child) button,
        .hcg-custom-theme th:not(:first-child) span { text-align: right !important; justify-content: flex-end !important; }

        /* Per-column cell widths */
%s

        /* Scroll height */
        %s
        ',
        max_width,
        size_settings$body_font_size,
        size_settings$title_font_size,
        size_settings$control_gap,
        size_settings$select_font_size,
        size_settings$select_pad_v,
        size_settings$select_pad_h,
        size_settings$body_font_size,
        header_color,
        col_filter_css,
        size_settings$menu_button_size,
        size_settings$menu_button_size,
        size_settings$menu_font_size,
        size_settings$menu_font_size,
        size_settings$body_font_size,
        size_settings$cell_pad_v,
        size_settings$cell_pad_h,
        header_color,
        size_settings$body_font_size,
        size_settings$cell_pad_v,
        size_settings$cell_pad_h,
        stripe_color,
        col_cell_css,
        if (!is.null(scroll_height))
          sprintf("#datagrid-container { flex: 0 0 %s !important; }", scroll_height)
        else ""
      )))
    ),
    
    tags$body(
      if (!is.null(title)) tags$h4(title),
      
      if (!is.null(select_control) || !is.null(export_controls)) {
        tags$div(
          class = "hc-table-controls",
          if (!is.null(select_control))  tags$div(class = "hc-table-select-wrap",  select_control),
          if (!is.null(export_controls)) tags$div(class = "hc-table-export-wrap", export_controls)
        )
      },
      
      if (isTRUE(filter_cols)) filter_html_block,
      
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