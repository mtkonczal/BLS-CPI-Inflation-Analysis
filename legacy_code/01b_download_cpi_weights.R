# ==============================================================================
# CPI weights straight from the BLS flat-file directory
#
# Replaces the hand-maintained weights/inflation_weights.csv with a programmatic
# pull of MONTHLY relative importance from:
#   https://download.bls.gov/pub/time.series/cu/cu.aspect   (CPI-U)
#   https://download.bls.gov/pub/time.series/cw/cw.aspect   (CPI-W)
#
# Docs: https://www.bls.gov/cpi/factsheets/using-cpi-metadata-aspect-files.htm
#
# aspect_type codes:
#   I   Relative importance                 CPI-U & CPI-W   Mar 2012 -> present
#   I1  End-of-year relative importance     CPI-U & CPI-W   Dec 2020 -> present
#   F   Seasonal factor                     CPI-U & CPI-W
#   W1  1-month effect on all items         CPI-U           Mar 2012 -> present
#   WC  12-month effect on all items        CPI-U           Mar 2012 -> present
#   V1  Prior 1-month percent change        CPI-U
#   VC  Prior 12-month percent change       CPI-U
#   M1  Median standard error, 1-month      CPI-U
#   MC  Median standard error, 12-month     CPI-U
#   H1  1-month "largest/smallest since"    CPI-U
#   HC  12-month "largest/smallest since"   CPI-U
#
# Key point: this is MONTHLY relative importance, keyed on series_id, refreshed
# with every CPI release. No manual download, no name matching.
# ==============================================================================

library(tidyverse)
library(lubridate)
library(httr)

BLS_CONTACT <- "rortybomb@gmail.com" # BLS blocks requests without a contact UA

# ------------------------------------------------------------------ downloader
download_bls_flatfile <- function(path,
                                  contact = BLS_CONTACT,
                                  dest = tempfile(fileext = ".txt")) {
  url <- paste0("https://download.bls.gov/pub/time.series/", path)
  resp <- httr::GET(
    url,
    httr::user_agent(paste0("R CPI research (", contact, ")")),
    httr::write_disk(dest, overwrite = TRUE),
    httr::timeout(600)
  )
  httr::stop_for_status(resp)
  dest
}

# --------------------------------------------------------------- aspect loader
# cu.aspect is ~31 MB. Cached locally; set refresh = TRUE on CPI release day.
get_cpi_aspects <- function(survey = c("cu", "cw"),
                            contact = BLS_CONTACT,
                            cache_dir = "data",
                            refresh = FALSE) {
  survey <- match.arg(survey)
  cache <- file.path(cache_dir, paste0(survey, "_aspect.rds"))
  if (!refresh && file.exists(cache)) {
    return(read_rds(cache))
  }

  f <- download_bls_flatfile(paste0(survey, "/", survey, ".aspect"), contact)

  raw <- read_tsv(
    f,
    col_types = cols(.default = col_character()),
    trim_ws = TRUE,
    progress = FALSE
  )
  names(raw) <- str_trim(names(raw))

  out <- raw %>%
    transmute(
      series_id   = str_trim(series_id),
      year        = as.integer(year),
      period      = str_trim(period),
      aspect_type = str_trim(aspect_type),
      value       = suppressWarnings(as.numeric(value))
    ) %>%
    filter(str_starts(period, "M"), period != "M13") %>%
    mutate(
      # CPI series_id layout: CU | seasonal(1) | periodicity(1) | area(4) | item(rest)
      seasonal  = str_sub(series_id, 3, 3),
      area_code = str_sub(series_id, 5, 8),
      item_code = str_sub(series_id, 9),
      date      = ymd(paste0(year, "-", str_sub(period, 2, 3), "-01"))
    )

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  write_rds(out, cache, compress = "xz")
  out
}

# ------------------------------------------------------- monthly rel-importance
# Relative importance is defined on the NOT-seasonally-adjusted series, so we
# join on area_code + item_code and deliberately ignore the seasonal flag.
get_cpi_weights_monthly <- function(aspects, area = "0000") {
  aspects %>%
    filter(aspect_type == "I", area_code == area) %>%
    distinct(area_code, item_code, year, period, .keep_all = TRUE) %>%
    select(area_code, item_code, year, period, date, weight = value)
}

# BLS's own contribution decomposition -- worth using instead of rolling your own
get_cpi_effects <- function(aspects, area = "0000") {
  aspects %>%
    filter(aspect_type %in% c("W1", "WC"), area_code == area) %>%
    select(area_code, item_code, year, period, date, aspect_type, value) %>%
    pivot_wider(
      names_from = aspect_type,
      values_from = value
    ) %>%
    rename(effect_1m = W1, effect_12m = WC)
}

# ------------------------------------------------------------- attach to cpi_data
# `weight`      = relative importance in the same month as the index observation
# `weight_lag`  = prior-month RI, which is the correct base for a 1-month
#                 contribution (contribution_t ~ weight_{t-1} * pct_change_t)
attach_cpi_weights <- function(cpi_data,
                               weights_monthly,
                               fallback = c("carry_forward", "none")) {
  fallback <- match.arg(fallback)

  if (!"item_code" %in% names(cpi_data)) {
    cpi_data <- cpi_data %>%
      mutate(item_code = str_sub(series_id, 9))
  }
  if (!"area_code" %in% names(cpi_data)) {
    cpi_data <- cpi_data %>%
      mutate(area_code = str_sub(series_id, 5, 8))
  }

  out <- cpi_data %>%
    left_join(
      weights_monthly %>% select(area_code, item_code, year, period, weight),
      by = c("area_code", "item_code", "year", "period")
    )

  if (fallback == "carry_forward") {
    # Only matters pre-Mar-2012, where cu.aspect has no coverage. Back-fills each
    # item with its earliest observed RI so old charts don't silently go blank.
    earliest <- weights_monthly %>%
      group_by(area_code, item_code) %>%
      slice_min(date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(area_code, item_code, weight_earliest = weight)

    out <- out %>%
      left_join(earliest, by = c("area_code", "item_code")) %>%
      mutate(
        weight_is_imputed = is.na(weight) & !is.na(weight_earliest),
        weight = coalesce(weight, weight_earliest)
      ) %>%
      select(-weight_earliest)
  }

  out %>%
    group_by(series_id) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(weight_lag = lag(weight)) %>%
    ungroup()
}

# ================================ VALIDATION ==================================
# Run this once before you trust the swap. Three checks:
#   1. Coverage: does cu.aspect actually start Mar 2012 and run to latest month?
#   2. Additivity: do the component RIs sum to the parent (All items = 100)?
#   3. Agreement: do December values match weights/inflation_weights.csv?
# ==============================================================================
validate_cpi_weights <- function(aspects, legacy_csv = "weights/inflation_weights.csv") {
  w <- get_cpi_weights_monthly(aspects)

  cat("---- 1. coverage ----\n")
  cat("aspect_type codes present:", paste(sort(unique(aspects$aspect_type)), collapse = ", "), "\n")
  aspects %>%
    group_by(aspect_type) %>%
    summarise(
      first = min(date), last = max(date),
      n_series = n_distinct(series_id), n_rows = n(), .groups = "drop"
    ) %>%
    arrange(aspect_type) %>%
    print(n = 30)

  cat("\n---- 2. additivity (All items should be 100 every month) ----\n")
  w %>%
    filter(item_code == "SA0") %>%
    summarise(min = min(weight), max = max(weight), n = n()) %>%
    print()

  cat("\nMajor groups summed (SAF SAH SAA SAT SAM SAR SAE SAG), should be ~100:\n")
  w %>%
    filter(item_code %in% c("SAF", "SAH", "SAA", "SAT", "SAM", "SAR", "SAE", "SAG")) %>%
    group_by(date) %>%
    summarise(total = sum(weight), n_items = n(), .groups = "drop") %>%
    slice_max(date, n = 6) %>%
    print()

  cat("\n---- 3. agreement with the hand-built CSV ----\n")
  if (!file.exists(legacy_csv)) {
    cat("legacy csv not found, skipping\n")
    return(invisible(w))
  }
  legacy <- read_csv(legacy_csv, show_col_types = FALSE) %>%
    filter(!is.na(weight))

  # The CSV labels a year Y with the RI table dated December of Y-1.
  # Compare on that convention.
  cat("Spot check -- Dec of prior year vs csv year_weight:\n")
  spot <- tribble(
    ~item_name, ~item_code,
    "All items", "SA0",
    "Food", "SAF1",
    "Energy", "SA0E",
    "Shelter", "SAH1",
    "Gasoline (all types)", "SETB01",
    "New vehicles", "SETA01",
    "Used cars and trucks", "SETA02"
  )
  legacy %>%
    inner_join(spot, by = "item_name") %>%
    mutate(dec_date = ymd(paste0(year_weight - 1, "-12-01"))) %>%
    left_join(
      w %>% select(item_code, dec_date = date, aspect_weight = weight),
      by = c("item_code", "dec_date")
    ) %>%
    mutate(diff = round(weight - aspect_weight, 4)) %>%
    select(item_name, year_weight, dec_date, csv = weight, aspect = aspect_weight, diff) %>%
    arrange(item_name, year_weight) %>%
    print(n = 40)

  invisible(w)
}

# ================================== USAGE =====================================
# aspects <- get_cpi_aspects("cu", refresh = TRUE)
# validate_cpi_weights(aspects)
#
# cpi_weights <- get_cpi_weights_monthly(aspects)
# cpi_data    <- attach_cpi_weights(cpi_data, cpi_weights)
#
# # BLS's own published contributions, as a check on yours:
# effects <- get_cpi_effects(aspects)
# ==============================================================================
