# ============================================================================
# LOAD PACKAGES
# ============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(haven)
library(zip)
library(shinyWidgets)
library(DBI)
library(RSQLite)

# ============================================================================
# LOAD INDEX FILES (small, loaded at startup)
# ============================================================================

table_index    <- readRDS("data/table_index.rds")
codelist_index <- readRDS("data/codelist_index.rds")

# ============================================================================
# HELPER: SQLite batch query for variable values
# ============================================================================

get_multiple_variable_values <- function(con, selections) {
  empty <- tibble(tablename = character(), varname = character(),
                  value = character(), label = character(), is_range = logical())
  if (nrow(selections) == 0) return(empty)
  
  # Use a temp table to avoid SQLite's expression depth limit 
  dbExecute(con, "CREATE TEMP TABLE IF NOT EXISTS _sel (tablename TEXT, varname TEXT)")
  dbExecute(con, "DELETE FROM _sel")
  dbWriteTable(con, "_sel", selections %>% mutate(across(everything(), as.character)),
               append = TRUE, row.names = FALSE)
  
  #  Handle special status variables (_NOT INCLUDED_ / _RESTRICTED_)
  special_vars <- empty
  has_regular <- TRUE
  if ("variable_status" %in% dbListTables(con)) {
    status_result <- dbGetQuery(con, "
      SELECT DISTINCT v.tablename, v.varname, vs.status_code
      FROM variables v
      INNER JOIN _sel s ON v.tablename = s.tablename AND v.varname = s.varname
      LEFT JOIN variable_status vs ON v.variable_id = vs.variable_id
    ") %>% as_tibble()
    
    special_vars <- status_result %>%
      filter(!is.na(status_code)) %>%
      mutate(value = if_else(status_code == 1L, "_NOT INCLUDED_", "_RESTRICTED_"),
             label = value, is_range = FALSE) %>%
      select(tablename, varname, value, label, is_range)
    
    if (nrow(special_vars) > 0) {
      regular <- status_result %>% filter(is.na(status_code))
      if (nrow(regular) == 0) {
        dbExecute(con, "DELETE FROM _sel")
        return(special_vars)
      }
      # Narrow temp table to only regular variables
      dbExecute(con, "DELETE FROM _sel")
      dbWriteTable(con, "_sel", regular %>% select(tablename, varname) %>%
                     mutate(across(everything(), as.character)),
                   append = TRUE, row.names = FALSE)
    }
  }
  
  # Query regular code values
  result <- dbGetQuery(con, "
    SELECT DISTINCT v.tablename, v.varname, cv.min_value, cv.max_value, cv.label
    FROM variables v
    INNER JOIN _sel s ON v.tablename = s.tablename AND v.varname = s.varname
    JOIN code_values cv ON v.variable_id = cv.variable_id
  ") %>% as_tibble()
  
  dbExecute(con, "DELETE FROM _sel")
  
  # De-intern labels if label_strings table exists
  if ("label_strings" %in% dbListTables(con) && nrow(result) > 0 &&
      any(grepl("^@\\d+$", result$label))) {
    lbl <- dbGetQuery(con, "SELECT label_id, label FROM label_strings") %>%
      as_tibble() %>%
      mutate(label_ref = paste0("@", label_id))
    result <- result %>%
      left_join(lbl %>% select(label_ref, actual = label), by = c("label" = "label_ref")) %>%
      mutate(label = coalesce(actual, label)) %>%
      select(tablename, varname, min_value, max_value, label)
  }
  
  # Expand ranges 
  parsed <- result %>%
    mutate(min_num = suppressWarnings(as.numeric(min_value)),
           max_num = suppressWarnings(as.numeric(max_value)),
           is_range = !is.na(min_num) & !is.na(max_num) & min_value != max_value)
  
  non_ranges <- parsed %>%
    filter(!is_range) %>%
    transmute(tablename, varname, value = min_value, label, is_range = FALSE)
  
  if (any(parsed$is_range)) {
    ranges <- parsed %>% filter(is_range)
    expanded_ranges <- ranges %>%
      mutate(len = as.integer(max_num - min_num + 1)) %>%
      tidyr::uncount(len) %>%
      group_by(tablename, varname, min_num, max_num, label) %>%
      mutate(num_val = min_num + row_number() - 1L,
             value = as.character(num_val),
             label = if_else(label == "Same label", value, label)) %>%
      ungroup() %>%
      transmute(tablename, varname, value, label, is_range = TRUE)
  } else {
    expanded_ranges <- empty
  }
  
  bind_rows(special_vars, expanded_ranges, non_ranges) %>%
    mutate(value = na_if(value, "<NA_VALUE>"),
           label = na_if(label, "<NA_LABEL>"))
}

# ============================================================================
# HELPER: Apply format type to a column
# ============================================================================

apply_format_type <- function(col_data, fmt) {
  if (is.null(fmt) || is.na(fmt) || fmt != "Numeric") return(as.character(col_data))
  num <- suppressWarnings(as.numeric(col_data))
  if (any(!is.na(col_data) & is.na(num))) return(as.character(col_data))
  num
}

# ============================================================================
# HELPER: Partition a data frame by table mapping, CORENO first
# ============================================================================

partition_by_table <- function(data, tbl_map) {
  split(tbl_map$colname, tbl_map$tablename) %>%
    map(~ {
      df <- data[, .x, drop = FALSE]
      if ("CORENO" %in% names(df)) df <- df[, c("CORENO", setdiff(names(df), "CORENO")), drop = FALSE]
      df
    }) %>%
    keep(~ nrow(.x) > 0)
}

# ============================================================================
# HELPER: Write a data frame to file in the chosen format
# ============================================================================

write_data_file <- function(df, filepath, ext) {
  switch(ext,
         csv = write.csv(df, filepath, row.names = FALSE),
         rds = saveRDS(df, filepath),
         dta = haven::write_dta(df, filepath),
         sav = haven::write_sav(df, filepath),
         stop("Unsupported file format")
  )
  stopifnot(file.exists(filepath))
  filepath
}

# ============================================================================
# HELPER: Output column/variable name — lowercase the variable name while
#         keeping the capitalised "_LIDS" suffix (e.g. "VAR1" -> "var1_LIDS")
# ============================================================================

make_lids_name <- function(x) paste0(tolower(x), "_LIDS")

# ============================================================================
# MULTI-ENTRY TABLES
# A few ONS LS tables can legitimately hold more than one record per LS member
# (e.g. several non-LS household members, or repeated event registrations such
# as cancers or births). For these tables LIDS generates a random number of
# rows per CORENO (1..MAX), with every CORENO keeping at least one row, capped
# at an overall per-table row ceiling. Every other table keeps exactly one row
# per CORENO, exactly as before.
# ============================================================================

MULTI_ENTRY_TABLES <- c("NM71", "NM81", "NM91", "NM01", "NM11", "NM21",
                        "EMBR", "CANC", "LBSM", "SBSM", "IDMI", "WDOW",
                        "ENLS", "REEN", "LBSF", "SBSF")
MULTI_ENTRY_MAX_PER_CORENO <- 5L
MULTI_ENTRY_ROW_CEILING    <- 600000L

# Rebuild one table so each CORENO has a random number of rows in
# 1..max_per_coreno, capped so the table holds at most row_ceiling rows.
# `corenos` is the canonical one-row-per-member identifier vector (shared across
# tables, so linkage is preserved); `varcols` are the table's selected
# variables. Values for every row are drawn independently from each variable's
# code list, exactly as in the base single-row draw, so each generated record
# stays an independent "impossible" event.
build_multi_entry_table <- function(corenos, varcols, tbl, loaded, fmt,
                                    include_all    = FALSE,
                                    max_per_coreno = MULTI_ENTRY_MAX_PER_CORENO,
                                    row_ceiling    = MULTI_ENTRY_ROW_CEILING) {
  ncoreno <- length(corenos)
  if (ncoreno == 0L) {
    out <- data.frame(CORENO = numeric(0), stringsAsFactors = FALSE, check.names = FALSE)
    for (vn in varcols) out[[vn]] <- character(0)
    return(out)
  }

  # Each CORENO gets a base row plus 0..(max-1) random extra rows.
  extras <- sample.int(max_per_coreno, ncoreno, replace = TRUE) - 1L
  budget <- row_ceiling - ncoreno                       # extra rows the ceiling allows
  if (budget < 0L) {
    extras[] <- 0L                                       # base rows alone already fill the ceiling
  } else if (sum(extras) > budget) {
    # Randomly drop extra rows down to the budget; each CORENO stays within 0..(max-1).
    slot   <- rep.int(seq_len(ncoreno), extras)
    keep   <- sample.int(length(slot), budget)
    extras <- tabulate(slot[keep], nbins = ncoreno)
  }
  counts <- 1L + extras
  total  <- sum(counts)

  out <- data.frame(CORENO = as.numeric(rep.int(corenos, counts)),
                    stringsAsFactors = FALSE, check.names = FALSE)
  for (vn in varcols) {
    key <- paste0(tbl, "::", vn)
    vt  <- loaded[[key]]
    if (is.null(vt) || nrow(vt) == 0) {
      out[[vn]] <- rep(NA, total)
    } else {
      vals         <- unique(vt$value)
      is_range_var <- all(vt$is_range)
      replace_flag <- is_range_var || !(include_all && length(vals) >= total)
      out[[vn]]    <- apply_format_type(sample(vals, total, replace = replace_flag), fmt[[key]])
    }
  }
  out
}

# ============================================================================
# PRE-COMPUTE TABLE CHOICES
# ============================================================================

ordered_tables <- table_index %>% arrange(.table_order)

table_choices <- setNames(ordered_tables$tablename, ordered_tables$tablename)

table_choices_display <- ifelse(
  !is.na(ordered_tables$tabledesc) & ordered_tables$tabledesc != "",
  paste0(ordered_tables$tablename, "  -  ", ordered_tables$tabledesc),
  ordered_tables$tablename
)

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "CeLSIUS Longitudinal Impossible Dataset (LIDS) Generator",
    titleWidth = "520",
    dropdownMenuOutput("custom_notification_menu"),
    tags$li(
      class = "dropdown",
      tags$a(href = "https://www.ucl.ac.uk/population-health-sciences/epidemiology-health-care/research/ucl-research-department-epidemiology-public-health/research/health-and-social-surveys-research-group/studies/celsius/longitudinal-impossible-dataset",
             target = "_blank", title = "Longitudinal Impossible Dataset (LIDS) information",
             style = "padding: 13px 14px;", icon("info-circle"))
    ),
    tags$li(
      class = "dropdown",
      tags$a(href = "#", class = "dropdown-toggle",
             "data-toggle" = "offcanvas", role = "button",
             title = "Toggle sidebar", icon("bars"))
    )
  ),
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Select",   tabName = "selecttab",   icon = icon("list")),
      menuItem("Generate", tabName = "generatetab",  icon = icon("cog")),
      menuItem("Preview",  tabName = "previewtab",   icon = icon("eye")),
      menuItem("Download", tabName = "downloadtab",  icon = icon("download"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,400;0,9..40,500;0,9..40,600;0,9..40,700;1,9..40,400&display=swap",
        rel = "stylesheet"
      ),
      tags$script(HTML("
       Shiny.addCustomMessageHandler('spin_cog', function(message) {
         var icon = document.getElementById('gen_icon');
         if (icon) {
           icon.classList.add('fa-spin');
           setTimeout(function() { icon.classList.remove('fa-spin'); }, 1000);
         }
       });

       // Hide/show Step 2 based on table selection
       $(document).on('changed.bs.select', '#selected_tables', function(e) {
         var selected = $(this).val();
         var $container = $('#variable_selection_container');
         if (!selected || selected.length === 0) {
           $container.hide();
         } else {
           $container.show();
         }
       });

       // Update button text with selection count
       $(document).on('changed.bs.select', '#selected_tables, #selected_variables', function(e) {
         var $picker = $(this);
         var selected = $picker.val();
         var inputId = $picker.attr('id');
         if (!selected || selected.length === 0) return;
         var $button = $picker.siblings('.dropdown-toggle').find('.filter-option-inner-inner');
         if (inputId === 'selected_tables') {
           $button.text('Click  to select tables... (' + selected.length + ' selected)');
         } else if (inputId === 'selected_variables') {
           $button.text('Click  to select variables... (' + selected.length + ' selected)');
         }
       });

       $(document).ready(function() {
         var initialSelected = $('#selected_tables').val();
         if (!initialSelected || initialSelected.length === 0) {
           $('#variable_selection_container').hide();
         }
         setTimeout(function() {
           $('#selected_tables, #selected_variables').trigger('changed.bs.select');
         }, 500);
       });

       // Single delegated handler for ALL deselect buttons 
       $(document).on('click', '[data-deselect]', function(e) {
         e.preventDefault();
         var action = $(this).data('deselect');
         Shiny.setInputValue('deselect_action', action, {priority: 'event'});
       });

       // Enforce max 200 variable selections client-side
       var MAX_VARS = 200;
       $(document).on('changed.bs.select', '#selected_variables', function(e) {
         var $picker = $(this);
         var selected = $picker.val() || [];
         if (selected.length > MAX_VARS) {
           // Revert to first MAX_VARS selections
           $picker.selectpicker('val', selected.slice(0, MAX_VARS));
           // Show a temporary toast via Shiny
           Shiny.setInputValue('_max_vars_exceeded', Math.random(), {priority: 'event'});
         }
       });
     ")),
      tags$style(HTML("
       :root {
         --ucl-dark-purple: #361a54;
         --ucl-mid-purple: #ba82ff;
         --color-bg: #f6f6f6;
         --color-surface: #ffffff;
         --color-border: #d8dae0;
         --color-border-subtle: #eceef1;
         --color-text: #24292f;
         --color-text-secondary: #57606a;
         --color-text-muted: #8b949e;
         --color-accent-hover: #4a2570;
         --color-success: #1a7f37;
         --color-success-bg: #dafbe1;
         --color-warning: #9a6700;
         --color-warning-bg: #fff8c5;
         --color-error: #cf222e;
         --color-error-bg: #ffebe9;
         --radius-sm: 4px;
         --radius-md: 6px;
         --radius-lg: 8px;
         --shadow-sm: 0 1px 2px rgba(27, 31, 36, 0.04);
         --shadow-md: 0 1px 3px rgba(27, 31, 36, 0.08), 0 1px 2px rgba(27, 31, 36, 0.06);
         --font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
       }
       html, body {
         margin: 0 !important; padding: 0 !important; height: 100%; overflow: hidden;
         font-family: var(--font-family) !important; font-size: 14px; line-height: 1.55;
         color: var(--color-text);
         -webkit-font-smoothing: antialiased; -moz-osx-font-smoothing: grayscale;
       }
       html { background: var(--ucl-dark-purple); }
       body { background: var(--color-bg) !important; }
       .wrapper { height: 100vh !important; max-height: 100vh !important; overflow: hidden !important; background: var(--color-bg) !important; }

       .main-header, .main-header .navbar, .main-header .navbar-custom-menu,
       .main-header .navbar-right, .main-header .logo,
       .skin-blue .main-header .logo, .skin-blue .main-header .navbar {
         background: var(--ucl-dark-purple) !important; border: none !important;
       }
       .main-header .logo {
         font-family: var(--font-family) !important; font-weight: 600 !important;
         font-size: 14px !important; letter-spacing: -0.01em;
         color: #ffffff !important; text-align: left !important;
       }
       .main-header .sidebar-toggle { display: none !important; }
       .main-header .navbar-nav > li > a {
         color: rgba(255,255,255,0.75) !important; padding: 13px 14px !important; font-size: 13px;
       }
       .main-header .navbar-nav > li > a:hover {
         background: rgba(255,255,255,0.07) !important; color: #fff !important;
       }
       .main-header .navbar-nav > li > a:focus,
       .main-header .navbar-nav > li > a:active {
         background: transparent !important; outline: none !important; box-shadow: none !important;
       }

       .main-sidebar, .left-side, .skin-blue .main-sidebar {
         background: var(--ucl-dark-purple) !important; box-shadow: none !important;
         border-right: 1px solid rgba(255,255,255,0.06);
         height: calc(100vh - 50px) !important; max-height: calc(100vh - 50px) !important;
         overflow-y: auto !important; overflow-x: hidden !important;
       }
       .sidebar-menu { padding: 10px 8px 0; }
       .sidebar-menu > li { margin: 1px 0; }
       .sidebar-menu > li > a {
         font-family: var(--font-family) !important; font-size: 13px !important;
         font-weight: 500 !important; color: rgba(255,255,255,0.65) !important;
         border-radius: var(--radius-md) !important; padding: 9px 12px !important;
         transition: background 0.12s ease, color 0.12s ease !important;
         border-left: none !important;
       }
       .sidebar-menu > li > a:hover {
         background: rgba(255,255,255,0.06) !important; color: rgba(255,255,255,0.9) !important;
       }
       .sidebar-menu > li.active > a {
         background: rgba(255,255,255,0.1) !important; color: #ffffff !important; font-weight: 600 !important;
       }
       .sidebar-menu > li.active > a::before, .sidebar-menu > li.active > a::after { display: none !important; }
       .sidebar-menu > li > a > .fa, .sidebar-menu > li > a > .fas, .sidebar-menu > li > a > .far {
         margin-right: 9px; width: 15px; text-align: center; font-size: 12px; opacity: 0.8;
       }

       .content-wrapper {
         background: var(--color-bg) !important; font-family: var(--font-family) !important;
         height: calc(100vh - 50px) !important; max-height: calc(100vh - 50px) !important;
         overflow-y: auto !important; overflow-x: hidden !important;
       }
       .content { padding: 20px 24px !important; }

       .box {
         border-radius: var(--radius-lg) !important; border: 1px solid var(--color-border) !important;
         box-shadow: var(--shadow-sm) !important; background: var(--color-surface) !important;
         margin-bottom: 20px;
       }
       .box-header {
         background: transparent !important;
         border-bottom: 1px solid var(--color-border-subtle) !important;
         padding: 14px 20px 12px !important;
       }
       .box-header.with-border { border-bottom: 1px solid var(--color-border-subtle) !important; }
       .box-title {
         font-family: var(--font-family) !important; font-size: 14px !important;
         font-weight: 600 !important; color: var(--color-text) !important; letter-spacing: -0.01em;
       }
       .box-body { padding: 20px !important; }
       .box.box-primary, .box.box-success { border-top: 2px solid var(--ucl-dark-purple) !important; }
       .box.box-primary > .box-header, .box.box-success > .box-header { background: transparent !important; }
       .box.box-success > .box-header .box-title,
       .box.box-primary > .box-header .box-title { color: var(--color-text) !important; }

       .form-control {
         font-family: var(--font-family) !important; border-radius: var(--radius-md) !important;
         border: 1px solid var(--color-border) !important; padding: 6px 10px !important;
         font-size: 13px !important; color: var(--color-text) !important;
         background: var(--color-surface) !important;
         transition: border-color 0.15s ease !important; box-shadow: none !important;
       }
       .form-control:focus {
         border-color: var(--ucl-mid-purple) !important;
         box-shadow: 0 0 0 2px rgba(186, 130, 255, 0.15) !important; outline: none !important;
       }
       .form-group label, .control-label {
         font-family: var(--font-family) !important; font-weight: 500 !important;
         color: var(--color-text) !important; margin-bottom: 4px !important; font-size: 13px !important;
       }
       input[type='number'] { border-radius: var(--radius-md) !important; }
       .checkbox label, .shiny-input-container .checkbox label {
         font-family: var(--font-family) !important; font-weight: 400 !important;
         color: var(--color-text-secondary) !important; font-size: 13px !important;
       }
       select.form-control { appearance: auto; }
       select.form-control option:checked, select.form-control option:hover,
       select option:checked, select option:active, select option:focus {
         background: #eceef1 linear-gradient(0deg, #eceef1 0%, #eceef1 100%) !important;
         background-color: #eceef1 !important; color: #24292f !important;
       }
       .selectize-input {
         font-family: var(--font-family) !important; border-radius: var(--radius-md) !important;
         border: 1px solid var(--color-border) !important; box-shadow: none !important;
       }
       .selectize-input.focus {
         border-color: var(--ucl-mid-purple) !important;
         box-shadow: 0 0 0 2px rgba(186, 130, 255, 0.15) !important;
       }
       .selectize-dropdown {
         font-family: var(--font-family) !important; border: 1px solid var(--color-border) !important;
         border-radius: var(--radius-md) !important; box-shadow: var(--shadow-md) !important;
       }
       .selectize-dropdown .active { background: var(--color-border-subtle) !important; color: var(--color-text) !important; }
       .selectize-dropdown .option:hover { background: var(--color-bg) !important; color: var(--color-text) !important; }

       .btn {
         font-family: var(--font-family) !important; border-radius: var(--radius-md) !important;
         font-weight: 500 !important; font-size: 13px !important;
         padding: 6px 14px !important; box-shadow: none !important;
       }
       .btn:focus { outline: none !important; box-shadow: none !important; }
       .btn:active {
         border-color: var(--ucl-mid-purple) !important;
         box-shadow: 0 0 0 2px rgba(186, 130, 255, 0.15) !important;
       }
       .btn-success, .btn-primary {
         background: var(--ucl-dark-purple) !important;
         border: 1px solid var(--ucl-dark-purple) !important; color: #ffffff !important;
       }
       .btn-default, .btn-light {
         background: var(--color-surface) !important;
         border: 1px solid var(--color-border) !important; color: var(--color-text) !important;
       }
       .btn-default:hover, .btn-light:hover { background: var(--color-bg) !important; border-color: #c0c4cb; }

       .bootstrap-select > .dropdown-toggle {
         font-family: var(--font-family) !important; border-radius: var(--radius-md) !important;
         border: 1px solid var(--color-border) !important; padding: 6px 10px !important;
         font-size: 13px !important; background: var(--color-surface) !important;
         color: var(--color-text) !important; box-shadow: none !important;
         transition: border-color 0.12s ease !important;
       }
       .bootstrap-select > .dropdown-toggle:focus,
       .bootstrap-select > .dropdown-toggle:active {
         border-color: var(--ucl-mid-purple) !important;
         box-shadow: 0 0 0 2px rgba(186, 130, 255, 0.15) !important; outline: none !important;
       }
       .bootstrap-select .dropdown-menu {
         font-family: var(--font-family) !important; border-radius: var(--radius-lg) !important;
         border: 1px solid var(--color-border) !important;
         box-shadow: var(--shadow-md), 0 8px 24px rgba(27, 31, 36, 0.08) !important;
         padding: 4px !important; margin-top: 4px !important;
         max-width: 100% !important; width: 100% !important;
         min-width: 0 !important; overflow: hidden !important;
       }
       .bootstrap-select .dropdown-menu li a {
         font-family: var(--font-family) !important; padding: 6px 10px !important;
         border-radius: var(--radius-sm) !important; margin: 1px 2px !important;
         font-size: 13px !important; color: var(--color-text) !important;
         overflow: hidden !important; text-overflow: ellipsis !important;
         white-space: nowrap !important; max-width: 100% !important; display: block !important;
       }
       .bootstrap-select .dropdown-menu li a:hover { background: var(--color-bg) !important; color: var(--color-text) !important; }
       .bootstrap-select .dropdown-menu li.selected a { background: var(--color-border-subtle) !important; color: var(--color-text) !important; }
       .bootstrap-select .dropdown-menu li.active a,
       .bootstrap-select .dropdown-menu li.active a:hover { background: transparent !important; color: var(--color-text) !important; }
       .bootstrap-select .dropdown-menu li a span.text {
         display: block !important; overflow: hidden !important;
         text-overflow: ellipsis !important; white-space: nowrap !important;
       }
       .bootstrap-select .dropdown-menu .dropdown-header {
         font-family: var(--font-family) !important; font-weight: 600 !important;
         color: var(--color-text-secondary) !important; background: var(--color-bg) !important;
         padding: 7px 10px 5px !important; font-size: 11px !important;
         border-radius: var(--radius-sm) !important; margin: 4px 2px 2px !important;
         text-transform: uppercase; letter-spacing: 0.04em;
       }
       .bs-searchbox { padding: 6px !important; }
       .bs-searchbox .form-control { border-radius: var(--radius-md) !important; font-size: 13px !important; }
       .bootstrap-select .dropdown-menu .inner {
         max-height: calc(80vh - 150px) !important; min-height: 200px !important;
         overflow-y: auto !important; overflow-x: hidden !important; max-width: 100% !important;
       }
       .bootstrap-select .dropdown-menu .inner > ul { max-height: none !important; overflow: visible !important; }
       @media (min-height: 900px) {
         .bootstrap-select .dropdown-menu .inner { max-height: min(calc(80vh - 150px), 600px) !important; }
       }
       @media (max-height: 500px) {
         .bootstrap-select .dropdown-menu .inner { max-height: calc(85vh - 100px) !important; min-height: 120px !important; }
       }
       .bootstrap-select { max-width: 100% !important; }
       .bootstrap-select .dropdown-menu li.selected a span.check-mark,
       .bootstrap-select .dropdown-menu li a span.glyphicon-ok,
       .bootstrap-select .dropdown-menu li.selected a::after { color: var(--ucl-dark-purple) !important; }

       .table-panel {
         margin-bottom: 10px !important; margin-left: 0 !important;
         padding: 12px 16px !important; border: 1px solid var(--color-border-subtle) !important;
         border-radius: var(--radius-md) !important; background: var(--color-surface) !important;
         border-left: 3px solid var(--ucl-dark-purple) !important;
       }
       .table-panel h4 { margin-top: 0 !important; margin-bottom: 4px !important; color: var(--color-text) !important; font-weight: 600 !important; font-size: 13px !important; }
       .table-panel-desc { color: var(--color-text-muted) !important; font-size: 12px !important; margin-bottom: 8px !important; font-style: normal !important; }
       .table-panel a[data-deselect]:hover,
       .table-panel a[target='_blank']:hover { color: var(--ucl-dark-purple) !important; }

       .selection-summary {
         background: var(--color-bg) !important; padding: 10px 14px !important;
         border-radius: var(--radius-md) !important; margin-bottom: 14px !important;
         font-size: 13px !important; border: 1px solid var(--color-border) !important;
         color: var(--color-text) !important;
       }
       .selection-summary strong { color: var(--ucl-dark-purple) !important; font-weight: 600 !important; }

       .shiny-notification {
         font-family: var(--font-family) !important; border-radius: var(--radius-md) !important;
         border: none !important; box-shadow: var(--shadow-md) !important;
         font-size: 13px !important; padding: 10px 14px !important;
       }
       .shiny-notification-message { background: var(--color-success-bg) !important; color: var(--color-success) !important; border-left: 3px solid var(--color-success) !important; }
       .shiny-notification-warning { background: var(--color-warning-bg) !important; color: var(--color-warning) !important; border-left: 3px solid var(--color-warning) !important; }
       .shiny-notification-error { background: var(--color-error-bg) !important; color: var(--color-error) !important; border-left: 3px solid var(--color-error) !important; }

       .custom-notif-item {
         background: var(--ucl-dark-purple) !important; color: rgba(255,255,255,0.8) !important;
         padding: 10px 12px !important; font-family: var(--font-family) !important;
         font-size: 12px !important; border-bottom: 1px solid rgba(255,255,255,0.06) !important;
       }
       .notif-dot { display: inline-block; width: 7px; height: 7px; margin-right: 6px; border-radius: 50%; flex-shrink: 0; }
       .notif-success { background: var(--color-success); }
       .notif-warning { background: var(--color-warning); }
       .notif-error { background: var(--color-error); }
       .notif-timestamp { display: block; text-align: right; margin-top: 4px; font-size: 11px; color: rgba(255,255,255,0.4); }
       .dropdown-menu > .header {
         background: var(--ucl-dark-purple) !important; color: #ffffff !important;
         font-family: var(--font-family) !important; font-weight: 500 !important;
         font-size: 12px !important; padding: 10px 12px !important;
         border-bottom: 1px solid rgba(255,255,255,0.08) !important;
       }
       .navbar-nav > .notifications-menu > .dropdown-menu {
         border-radius: var(--radius-lg) !important; border: 1px solid rgba(0,0,0,0.15) !important;
         box-shadow: var(--shadow-md) !important; overflow: hidden !important;
       }

       .dataTables_wrapper { font-family: var(--font-family) !important; font-size: 13px !important; }
       table.dataTable { border-collapse: collapse !important; }
       table.dataTable thead th {
         font-family: var(--font-family) !important; background: var(--color-bg) !important;
         color: var(--color-text) !important; font-weight: 600 !important;
         padding: 9px 12px !important; border-bottom: 1px solid var(--color-border) !important;
         font-size: 12px !important; text-transform: none; letter-spacing: 0;
       }
       table.dataTable tbody td {
         font-family: var(--font-family) !important; padding: 7px 12px !important;
         border-bottom: 1px solid var(--color-border-subtle) !important;
         color: var(--color-text) !important; font-size: 13px !important;
       }
       table.dataTable tbody tr:hover { background: var(--color-bg) !important; }
       .dataTables_filter input {
         font-family: var(--font-family) !important; border-radius: var(--radius-md) !important;
         border: 1px solid var(--color-border) !important; padding: 4px 8px !important; font-size: 13px !important;
       }
       .dataTables_filter input:focus {
         border-color: var(--ucl-mid-purple) !important;
         box-shadow: 0 0 0 2px rgba(186, 130, 255, 0.15) !important; outline: none !important;
       }
       .dataTables_paginate .paginate_button {
         font-family: var(--font-family) !important; border-radius: var(--radius-sm) !important;
         border: none !important; padding: 4px 10px !important; margin: 0 1px !important; font-size: 12px !important;
       }
       .dataTables_paginate .paginate_button.current {
         background: var(--color-border-subtle) !important; color: var(--color-text) !important;
         border: 1px solid var(--color-border) !important; font-weight: 600 !important;
       }
       .dataTables_paginate .paginate_button:hover {
         background: transparent !important; background-image: none !important; color: inherit !important;
       }
       .dataTables_info { font-family: var(--font-family) !important; font-size: 12px !important; color: var(--color-text-muted) !important; }

       .nav-tabs { border-bottom: 1px solid var(--color-border) !important; margin-bottom: 16px !important; }
       .nav-tabs > li > a {
         font-family: var(--font-family) !important; border: none !important;
         border-bottom: 2px solid transparent !important; border-radius: 0 !important;
         color: var(--color-text-secondary) !important; font-weight: 500 !important;
         font-size: 13px !important; padding: 8px 14px !important;
         margin-right: 0 !important; margin-bottom: -1px !important; background: transparent !important;
       }
       .nav-tabs > li > a:hover { background: transparent !important; color: var(--color-text) !important; border-bottom-color: var(--color-border) !important; }
       .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
         background: transparent !important; color: var(--color-text) !important;
         border: none !important; border-bottom: 2px solid var(--ucl-dark-purple) !important; font-weight: 600 !important;
       }

       .progress { border-radius: var(--radius-sm) !important; background: var(--color-border-subtle) !important; height: 5px !important; box-shadow: none !important; }
       .progress-bar { background: var(--ucl-dark-purple) !important; border-radius: var(--radius-sm) !important; }

       h4 { font-family: var(--font-family) !important; color: var(--color-text) !important; font-weight: 600 !important; font-size: 14px !important; letter-spacing: -0.01em; }
       p { font-family: var(--font-family) !important; color: var(--color-text-secondary) !important; line-height: 1.55 !important; font-size: 13px !important; }

       ::-webkit-scrollbar { width: 8px; height: 8px; }
       ::-webkit-scrollbar-track { background: var(--color-bg); }
       ::-webkit-scrollbar-thumb { background: var(--color-border); border-radius: 4px; }
       ::-webkit-scrollbar-thumb:hover { background: #b0b4bc; }
       .loading-indicator { padding: 20px; text-align: center; color: var(--color-text-muted); }
       .loading-indicator i { font-size: 24px; margin-bottom: 10px; display: block; }
       .dropdown-menu { font-family: var(--font-family) !important; }
     "))
    ),
    tabItems(
      tabItem(
        tabName = "selecttab",
        fluidRow(
          box(
            width = 12, title = "Select Variables", status = "primary", solidHeader = TRUE,
            
            # Step 1: Select Tables
            div(
              style = "margin-bottom: 24px;",
              h4("Step 1: Select Tables", style = "margin-top: 0;"),
              pickerInput(
                inputId = "selected_tables", label = NULL,
                choices = table_choices, multiple = TRUE,
                choicesOpt = list(content = table_choices_display),
                options = pickerOptions(
                  actionsBox = TRUE, liveSearch = TRUE,
                  liveSearchPlaceholder = "Search tables...",
                  selectedTextFormat = "static",
                  noneSelectedText = "Click  to select tables...",
                  dropupAuto = FALSE, size = FALSE
                ),
                width = "100%"
              )
            ),
            
            # Step 2: Select Variables (pickerInput placed statically, updated server-side)
            div(
              id = "variable_selection_container",
              h4("Step 2: Select Variables", style = "margin-top: 24px;"),
              pickerInput(
                inputId = "selected_variables", label = NULL,
                choices = NULL, multiple = TRUE,
                options = pickerOptions(
                  actionsBox = FALSE, liveSearch = TRUE,
                  liveSearchPlaceholder = "Search variables...",
                  selectedTextFormat = "static",
                  noneSelectedText = "Click  to select variables from selected tables...",
                  dropupAuto = FALSE, size = FALSE
                ),
                width = "100%"
              ),
              uiOutput("selected_vars_summary")
            )
          )
        )
      ),
      tabItem(
        tabName = "generatetab",
        fluidRow(
          box(
            width = 12, title = "Generate Dataset", status = "primary", solidHeader = TRUE,
            numericInput(min = 0, "n_obs", "Number of Observations:", value = 100),
            numericInput(min = 0, "seed", "Random Seed (optional):", value = NA),
            checkboxInput("include_all_values",
                          "Include all unique codes of selected variables",
                          value = FALSE),
            p("Click the button below to generate the dataset."),
            actionButton(
              "generate",
              span(icon("cog", class = "fa fa-cog", id = "gen_icon"), "Generate Dataset"),
              class = "btn-success generate-btn",
              style = "margin-top: 5px; color: white;"
            )
          )
        )
      ),
      tabItem(
        tabName = "previewtab",
        fluidRow(
          box(
            title = "Dataset Preview", status = "success", solidHeader = TRUE, width = 12,
            div(style = "overflow-x: auto; white-space: nowrap;", uiOutput("data_tables"))
          )
        )
      ),
      tabItem(
        tabName = "downloadtab",
        fluidRow(uiOutput("download_ui"))
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # SQLite connection (per session)
  db_con <- dbConnect(RSQLite::SQLite(), "data/codelist_values.db")
  session$onSessionEnded(function() try(dbDisconnect(db_con), silent = TRUE))
  onStop(function() try(dbDisconnect(db_con), silent = TRUE))
  
  # Reactive state 
  stored_notifications <- reactiveVal(tibble(type = character(), message = character(), timestamp = character()))
  show_labels    <- reactiveVal(FALSE)
  gen            <- reactiveValues(vars = NULL, tables = NULL, n_obs = NULL,
                                   include_all = NULL, validated_seed = NA, seed_valid = FALSE)
  loaded_values  <- reactiveValues(data = list())
  format_meta    <- reactiveValues(data = list())
  
  # Notification helper (DRY) 
  notify <- function(msg, type = "message", duration = 4) {
    notif_type <- switch(type, message = "success", type)
    showNotification(msg, type = type, duration = duration)
    stored_notifications(
      bind_rows(
        tibble(type = notif_type, message = msg, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        stored_notifications()
      ) %>% filter(!is.na(type), !is.na(message)) %>% head(8)
    )
  }
  
  # ==========================================================================
  # VARIABLE SELECTION: update picker choices when tables change
  # ==========================================================================
  
  observeEvent(input$selected_tables, {
    tables <- input$selected_tables
    if (is.null(tables) || length(tables) == 0) {
      updatePickerInput(session, "selected_variables", choices = list(), selected = character(0))
      return()
    }
    
    available_vars <- codelist_index %>%
      filter(tablename %in% tables) %>%
      arrange(.var_order)
    
    table_order <- table_index %>%
      filter(tablename %in% tables) %>%
      arrange(.table_order) %>%
      pull(tablename)
    
    # Build grouped choices + display content
    grouped_choices <- list()
    all_content <- character()
    
    for (tbl in table_order) {
      vt <- available_vars %>% filter(tablename == tbl)
      vals <- paste0(vt$tablename, "::", vt$varname)
      nms  <- paste0(vt$varname, " (", vt$tablename, ")")
      grouped_choices[[tbl]] <- setNames(vals, nms)
      all_content <- c(all_content, ifelse(
        !is.na(vt$shortdesc) & vt$shortdesc != "",
        paste0(vt$varname, " (", vt$tablename, ")  -  ", vt$shortdesc),
        paste0(vt$varname, " (", vt$tablename, ")")
      ))
    }
    
    # Preserve valid previous selections
    all_valid <- paste0(available_vars$tablename, "::", available_vars$varname)
    prev <- isolate(input$selected_variables)
    valid_sel <- if (!is.null(prev)) intersect(prev, all_valid) else character(0)
    
    updatePickerInput(
      session, "selected_variables",
      choices = grouped_choices,
      selected = valid_sel,
      choicesOpt = list(content = all_content)
    )
  }, ignoreNULL = FALSE)
  
  # Show notification when JS blocks selection beyond 200
  observeEvent(input$`_max_vars_exceeded`, {
    notify("Warning: Maximum of 200 variable selections allowed.", "warning")
  }, ignoreInit = TRUE)
  
  # ==========================================================================
  # SELECTED VARIABLES SUMMARY 
  # ==========================================================================
  
  output$selected_vars_summary <- renderUI({
    req(input$selected_variables, length(input$selected_variables) > 0)
    
    parts <- strsplit(input$selected_variables, "::", fixed = TRUE)
    sel <- tibble(tablename = vapply(parts, `[`, "", 1),
                  varname   = vapply(parts, `[`, "", 2))
    
    details <- codelist_index %>%
      inner_join(sel, by = c("tablename", "varname")) %>%
      arrange(.var_order)
    
    n_vars   <- nrow(sel)
    n_tables <- n_distinct(sel$tablename)
    
    tbl_order <- table_index %>%
      filter(tablename %in% sel$tablename) %>%
      arrange(.table_order) %>%
      pull(tablename)
    
    panels <- lapply(tbl_order, function(tbl) {
      vt <- details %>% filter(tablename == tbl) %>% arrange(.var_order)
      tbl_desc <- table_index$tabledesc[table_index$tablename == tbl][1]
      
      div(
        class = "table-panel",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          div(
            h4(style = "margin: 0; margin-bottom: 4px;", tbl),
            if (!is.na(tbl_desc)) div(class = "table-panel-desc", tbl_desc)
          ),
          # JS-driven deselect: data attribute carries the table name prefixed with "table::"
          tags$a(href = "#", `data-deselect` = paste0("table::", tbl),
                 style = "color: #c0c4cb; font-size: 14px; padding: 2px 6px; cursor: pointer;",
                 title = paste0("Deselect all variables from ", tbl),
                 icon("times"))
        ),
        div(
          style = "margin-top: 8px;",
          lapply(seq_len(nrow(vt)), function(i) {
            var_id  <- paste0(vt$tablename[i], "::", vt$varname[i])
            var_url <- vt$varurl[i]
            has_url <- !is.na(var_url) && var_url != ""
            div(
              style = "padding: 4px 0; border-bottom: 1px solid #eceef1; display: flex; justify-content: space-between; align-items: center;",
              div(style = "flex: 1;",
                  tags$strong(style = "font-size: 13px; font-weight: 500;", vt$varname[i]),
                  tags$span(style = "margin-left: 10px; color: #57606a; font-size: 12px;", vt$shortdesc[i])),
              div(
                style = "display: flex; align-items: center; gap: 4px;",
                if (has_url) tags$a(href = var_url, target = "_blank", icon("search"),
                                    style = "color: #c0c4cb; font-size: 14px; padding: 2px 6px; cursor: pointer; text-decoration: none;",
                                    title = "View in data dictionary"),
                # JS-driven deselect: data attribute carries "var::TABLE::VAR"
                tags$a(href = "#", `data-deselect` = paste0("var::", var_id),
                       style = "color: #c0c4cb; font-size: 14px; padding: 2px 6px; cursor: pointer;",
                       title = paste0("Deselect ", vt$varname[i]),
                       icon("times"))
              )
            )
          })
        )
      )
    })
    
    tagList(
      h4("Selected Variables by Table", style = "margin-top: 28px; margin-bottom: 12px;"),
      div(class = "selection-summary",
          icon("info-circle"), " ",
          strong(n_vars), paste0(ifelse(n_vars > 1, " variables", " variable"), " selected from "),
          strong(n_tables), ifelse(n_tables > 1, " tables", " table")),
      panels
    )
  })
  
  # ==========================================================================
  # SINGLE DESELECT HANDLER 
  # ==========================================================================
  
  observeEvent(input$deselect_action, {
    action <- input$deselect_action
    current <- input$selected_variables
    if (is.null(current)) return()
    
    if (startsWith(action, "table::")) {
      tbl <- sub("^table::", "", action)
      new_sel <- current[!startsWith(current, paste0(tbl, "::"))]
    } else if (startsWith(action, "var::")) {
      var_id <- sub("^var::", "", action)
      new_sel <- setdiff(current, var_id)
    } else {
      return()
    }
    updatePickerInput(session, "selected_variables", selected = new_sel)
  }, ignoreInit = TRUE)
  
  # ==========================================================================
  # COLLECT SELECTED VARIABLES
  # ==========================================================================
  
  selected_vars_flat <- reactive({
    sv <- input$selected_variables
    if (is.null(sv) || length(sv) == 0) return(NULL)
    parts <- strsplit(sv, "::", fixed = TRUE)
    tibble(tablename = vapply(parts, `[`, "", 1),
           varname   = vapply(parts, `[`, "", 2))
  })
  
  # ==========================================================================
  # GENERATION LOGIC
  # ==========================================================================
  
  observeEvent(input$generate, {
    withProgress(message = "Generating dataset...",
                 detail = "Please wait while your impossible dataset is generated.", value = 0, {
                   
                   incProgress(0.1, detail = "Validating selections...")
                   
                   vars_df <- selected_vars_flat()
                   if (is.null(vars_df) || nrow(vars_df) == 0) {
                     notify("Error: Please select at least one variable before generating.", "error")
                     return()
                   }
                   
                   # Validate seed
                   seed_value <- input$seed
                   validated_seed <- NA
                   if (!is.na(seed_value)) {
                     max_seed <- .Machine$integer.max
                     if (seed_value != floor(seed_value) || seed_value < 0 || seed_value > max_seed) {
                       notify(paste0("Error: Random seed must be a whole number between 0 and ", max_seed, "."), "error", 6)
                       return()
                     }
                     validated_seed <- as.integer(seed_value)
                   }
                   
                   sel     <- vars_df$varname
                   sel_tab <- vars_df$tablename
                   
                   # CORENO handling
                   coreno_needed <- codelist_index %>%
                     filter(tablename %in% sel_tab, varname == "CORENO") %>%
                     distinct(tablename, varname) %>%
                     anti_join(vars_df, by = c("tablename", "varname"))
                   
                   if (nrow(coreno_needed) > 0) {
                     sel     <- c(sel, coreno_needed$varname)
                     sel_tab <- c(sel_tab, coreno_needed$tablename)
                     notify("Warning: CORENO was automatically added for table linkage.", "warning")
                   }

                   # Heads-up: selected tables that can hold several records per CORENO
                   multi_selected <- intersect(unique(sel_tab), MULTI_ENTRY_TABLES)
                   if (length(multi_selected) > 0) {
                     tbls <- if (length(multi_selected) > 3)
                       paste0(paste(multi_selected[1:3], collapse = ", "), " +", length(multi_selected) - 3, " more")
                     else paste(multi_selected, collapse = ", ")
                     notify(paste0("Warning: ", tbls, " may contain multiple records per CORENO."), "warning")
                   }

                   incProgress(0.2, detail = "Loading code values and format metadata...")
                   
                   selections <- tibble(tablename = sel_tab, varname = sel)
                   all_values <- get_multiple_variable_values(db_con, selections)
                   
                   if (nrow(all_values) == 0) {
                     notify("Error: Selected variables have no defined codes in the codelist.", "error")
                     return()
                   }
                   
                   # Cache values as named list of tibbles
                   loaded_values$data <- all_values %>%
                     group_by(tablename, varname) %>%
                     summarise(vals = list(tibble(value = value, label = label, is_range = is_range)), .groups = "drop") %>%
                     {setNames(.$vals, paste0(.$tablename, "::", .$varname))}
                   
                   # Cache format metadata as named character vector
                   format_meta$data <- codelist_index %>%
                     inner_join(selections, by = c("tablename", "varname")) %>%
                     {setNames(.$format, paste0(.$tablename, "::", .$varname))}
                   
                   # Calculate min required obs if include_all_values is checked
                   min_req <- 0
                   if (input$include_all_values) {
                     non_range <- all_values %>%
                       group_by(tablename, varname) %>% filter(!all(is_range)) %>% ungroup()
                     if (nrow(non_range) > 0) {
                       min_req <- non_range %>%
                         group_by(tablename, varname) %>%
                         summarise(n = n_distinct(value), .groups = "drop") %>%
                         pull(n) %>% max()
                     }
                   }
                   
                   validated_n <- max(min_req, min(input$n_obs, 550000))
                   if (validated_n != input$n_obs) {
                     updateNumericInput(session, "n_obs", value = validated_n)
                     msg <- if (validated_n < input$n_obs) {
                       paste0("Warning: Number of observations capped at ", validated_n, ", which is the maximum allowed.")
                     } else {
                       paste0("Warning: Number of observations set to ", validated_n,
                              ", which is the minimum required to include all selected codes.")
                     }
                     notify(msg, "warning")
                   }

                   # Heads-up: at this sample size the per-table row ceiling can be reached
                   if (length(multi_selected) > 0 &&
                       validated_n * MULTI_ENTRY_MAX_PER_CORENO >= MULTI_ENTRY_ROW_CEILING) {
                     notify(paste0("Warning: Multi-record tables may be capped at ",
                                   format(MULTI_ENTRY_ROW_CEILING, big.mark = ",", scientific = FALSE),
                                   " rows at this sample size."), "warning")
                   }

                   incProgress(0.4, detail = "Sampling values...")
                   
                   gen$n_obs           <- validated_n
                   gen$vars            <- sel
                   gen$tables          <- sel_tab
                   gen$include_all     <- input$include_all_values
                   gen$validated_seed  <- validated_seed
                   gen$seed_valid      <- TRUE
                   
                   incProgress(0.3, detail = "Finalizing dataset...")
                   notify("Success: Dataset generated.")
                   session$sendCustomMessage("spin_cog", list())
                 })
  })
  
  # ==========================================================================
  # DATA GENERATION 
  # ==========================================================================
  
  impossible_data <- reactive({
    req(gen$vars, gen$tables, length(loaded_values$data) > 0,
        length(format_meta$data) > 0, gen$seed_valid)
    
    if (!is.na(gen$validated_seed)) set.seed(gen$validated_seed)
    
    n       <- gen$n_obs
    sel     <- gen$vars
    sel_tab <- gen$tables
    coreno  <- sample.int(n)
    
    data_list <- lapply(seq_along(sel), function(i) {
      vn  <- sel[i]
      key <- paste0(sel_tab[i], "::", vn)
      
      if (vn == "CORENO") return(as.numeric(coreno))
      
      vt <- loaded_values$data[[key]]
      if (is.null(vt) || nrow(vt) == 0) return(rep(NA, n))
      
      vals <- unique(vt$value)
      is_range_var <- all(vt$is_range)
      replace_flag <- is_range_var || !(gen$include_all && length(vals) >= n)
      
      apply_format_type(sample(vals, n, replace = replace_flag), format_meta$data[[key]])
    })
    
    data <- setNames(as.data.frame(data_list, stringsAsFactors = FALSE), sel)
    attr(data, "table_map") <- tibble(colname = sel, tablename = sel_tab)
    data
  })
  
  # ==========================================================================
  # DATA PARTITIONING
  # ==========================================================================
  
  partitioned_data <- reactive({
    req(impossible_data())
    parts <- partition_by_table(impossible_data(), attr(impossible_data(), "table_map"))

    # Rebuild multi-entry tables with several rows per CORENO; all others are
    # left exactly as produced by the base draw above.
    multi <- intersect(names(parts), MULTI_ENTRY_TABLES)
    if (length(multi) > 0) {
      if (!is.na(gen$validated_seed)) set.seed(gen$validated_seed)
      for (tbl in multi) {
        df <- parts[[tbl]]
        if (!"CORENO" %in% names(df)) next            # no linkage key: leave unchanged
        parts[[tbl]] <- build_multi_entry_table(
          corenos     = df$CORENO,
          varcols     = setdiff(names(df), "CORENO"),
          tbl         = tbl,
          loaded      = loaded_values$data,
          fmt         = format_meta$data,
          include_all = gen$include_all
        )
      }
    }
    parts
  })
  
  # ==========================================================================
  # LABEL TRANSFORMATION (cached)
  # ==========================================================================
  
  label_lookups <- reactive({
    req(length(loaded_values$data) > 0)
    lapply(loaded_values$data, function(lm) {
      if (is.null(lm) || nrow(lm) == 0) return(NULL)
      lu <- lm %>% filter(!is.na(value)) %>% distinct(value, .keep_all = TRUE)
      if (nrow(lu) > 0) setNames(lu$label, lu$value) else NULL
    })
  })
  
  partitioned_data_labels <- reactive({
    req(length(loaded_values$data) > 0)
    parts   <- partitioned_data()        # same (already multi-entry-expanded) data
    lookups <- label_lookups()

    labelled <- map(names(parts), function(tbl) {
      df <- parts[[tbl]]
      for (col in names(df)) {
        if (col == "CORENO") next
        key    <- paste0(tbl, "::", col)
        lookup <- lookups[[key]]
        if (is.null(lookup) || length(lookup) == 0) next

        orig    <- df[[col]]
        col_chr <- as.character(orig)
        idx     <- match(col_chr, names(lookup))
        df[[col]] <- ifelse(!is.na(idx), lookup[idx], col_chr)
        df[[col]][is.na(orig)] <- NA
      }
      df
    })
    setNames(labelled, names(parts))
  })
  
  # ==========================================================================
  # PREVIEW RENDERING
  # ==========================================================================
  
  output$data_tables <- renderUI({
    if (is.null(gen$vars)) {
      return(div(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 80px 20px; text-align: center; min-height: 240px;",
        icon("eye", style = "font-size: 64px; color: #d8dae0; margin-bottom: 20px;"),
        p(style = "color: #8b949e; font-size: 14px; max-width: 400px;",
          HTML('Please generate the dataset first in the <strong><i class="fa fa-cog"></i> Generate</strong> tab.'))
      ))
    }
    
    tabs <- map(names(partitioned_data()), ~ tabPanel(title = .x, DTOutput(paste0("table_", .x))))
    tagList(
      div(style = "margin-bottom: 15px;",
          actionButton("toggle_labels", uiOutput("toggle_label_ui"),
                       class = "btn btn-light", style = "border: 1px solid #d8dae0; padding: 6px 12px;"),
          span(style = "margin-left: 15px; color: #57606a; font-size: 12px;",
               "(Preview shows max 100 random rows per table)")),
      do.call(tabsetPanel, tabs)
    )
  })
  
  output$toggle_label_ui <- renderUI({
    txt <- if (show_labels()) "Show Values" else "Show Labels"
    tagList(icon("table"), span(txt, style = "margin-left: 6px;"))
  })
  
  observeEvent(input$toggle_labels, show_labels(!show_labels()))
  
  observe({
    if (is.null(gen$vars)) return()
    data_list <- if (show_labels()) partitioned_data_labels() else partitioned_data()

    set.seed(42)
    walk(names(data_list), function(nm) {
      local({
        df_full <- data_list[[nm]]
        n_rows  <- nrow(df_full)
        idx     <- if (n_rows > 100) sort(sample.int(n_rows, 100)) else seq_len(n_rows)
        df      <- df_full[idx, , drop = FALSE]
        output[[paste0("table_", nm)]] <- renderDT(
          datatable(df, options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE,
                                       deferRender = TRUE, scroller = TRUE, dom = "ftp",
                                       columnDefs = list(list(className = "dt-center", targets = "_all"))),
                    rownames = FALSE),
          server = TRUE
        )
      })
    })
  })
  
  # ==========================================================================
  # DOWNLOAD
  # ==========================================================================
  
  output$download_ui <- renderUI({
    if (is.null(gen$vars)) {
      box(title = "Dataset Download", status = "success", solidHeader = TRUE, width = 12,
          div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 80px 20px; text-align: center; min-height: 240px;",
              icon("download", style = "font-size: 64px; color: #d8dae0; margin-bottom: 20px;"),
              p(style = "color: #8b949e; font-size: 14px; max-width: 400px;",
                HTML('Please generate the dataset first in the <strong><i class="fa fa-cog"></i> Generate</strong> tab.'))))
    } else {
      box(title = "Dataset Download", status = "success", solidHeader = TRUE, width = 12,
          selectInput("file_format", "Select file format:",
                      choices = c("CSV" = "csv", "RDS" = "rds", "DTA" = "dta", "SAV" = "sav"), selected = "csv"),
          checkboxInput("include_codelist", "Include a code list for the selected variables", value = FALSE),
          p("Click the button below to download the generated dataset as a ZIP file."),
          downloadButton("downloadData", "Download ZIP"))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("LIDS_tables_", Sys.Date(), ".zip"),
    content = function(file) {
      withProgress(message = "Preparing download...", value = 0, {
        tmp_dir <- tempfile("impossible_")
        dir.create(tmp_dir)
        
        data_parts <- isolate(partitioned_data())
        stopifnot(!is.null(data_parts), length(data_parts) > 0)
        ext <- input$file_format
        
        incProgress(0.3, detail = "Writing files...")
        
        files <- map_chr(names(data_parts), function(nm) {
          df <- data_parts[[nm]]
          stopifnot(!is.null(df), nrow(df) > 0)
          colnames(df) <- make_lids_name(colnames(df))
          write_data_file(df, file.path(tmp_dir, paste0(nm, "_LIDS.", ext)), ext)
        })
        
        # Add codelist file if requested
        if (input$include_codelist) {
          incProgress(0.1, detail = "Creating code list file...")
          
          codelist_df <- map_dfr(names(loaded_values$data), function(key) {
            parts <- strsplit(key, "::", fixed = TRUE)[[1]]
            vl <- loaded_values$data[[key]]
            fmt <- format_meta$data[[key]] %||% NA_character_
            
            if (is.null(vl) || nrow(vl) == 0) return(NULL)
            vl <- vl %>% filter(is.na(value) | is.na(label) | value != label)
            if (nrow(vl) == 0) return(NULL)
            
            vl %>% transmute(
              tablename_LIDS = paste0(parts[1], "_LIDS"),
              varname_LIDS   = make_lids_name(parts[2]),
              format = fmt, value, label
            )
          })
          
          if (!is.null(codelist_df) && nrow(codelist_df) > 0) {
            files <- c(files,
                       write_data_file(codelist_df, file.path(tmp_dir, paste0("codelist_LIDS.", ext)), ext))
          }
        }
        
        incProgress(0.4, detail = "Creating ZIP archive...")
        zip::zipr(zipfile = file, files = files, recurse = FALSE)
        incProgress(0.1, detail = "Complete!")
      })
    },
    contentType = "application/zip"
  )
  
  # ==========================================================================
  # NOTIFICATIONS
  # ==========================================================================
  
  output$custom_notification_menu <- renderMenu({
    notifs <- stored_notifications()
    if (nrow(notifs) == 0) return(NULL)
    
    items <- pmap(notifs, function(type, message, timestamp) {
      dot_class <- paste("notif-dot", switch(type, success = "notif-success",
                                             warning = "notif-warning",
                                             error = "notif-error", "notif-success"))
      tags$li(
        class = "custom-notif-item",
        div(style = "display: flex; align-items: flex-start;",
            tags$span(class = dot_class, style = "margin-top: 4px;"),
            div(style = "margin-left: 10px; flex: 1;", message)),
        tags$div(class = "notif-timestamp", paste0("Timestamp: ", timestamp))
      )
    })
    
    dropdownMenu(type = "notifications", icon = icon("bell"), .list = items,
                 headerText = HTML('<i class="fa fa-bell"></i> Notifications'), badgeStatus = NULL)
  })
}

shinyApp(ui, server)