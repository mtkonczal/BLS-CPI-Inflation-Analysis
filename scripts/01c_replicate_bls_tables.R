# ==============================================================================
# Replicate BLS CPI news release Tables 6 and 7 from the flat files.
#
#   Table 6  https://www.bls.gov/news.release/cpi.t06.htm   1-month analysis
#   Table 7  https://www.bls.gov/news.release/cpi.t07.htm  12-month analysis
#
# Both tables are relative importance, percent change, effect on all items, and
# median standard error, item by item. Everything in them is reconstructible
# from cu.data.0.Current plus cu.aspect, so they are the natural end-to-end test
# of the weights pipeline: if the dating convention or the join key is wrong,
# these checks fail immediately and by a visible margin.
#
# Run on release day, after scripts/01_download_cpi_data.R:
#   source("scripts/01c_replicate_bls_tables.R")
#   results <- replicate_bls_tables(cpi_data)
#
# Baseline, June 2026 release (2026-08-11):
#   relative importance      T6 307/307  T7 307/307   exact
#   1-month percent change   T6 371/371               exact to published 1 dp
#   12-month percent change  T7 371/371               exact to published 1 dp
#   effect on all items      T6 269/269  T7 306/306   exact
#   median standard error    T6 318/318  T7 372/372   exact
#
# Two things this file pins down that are easy to get wrong:
#
# 1. DATING. A cu.aspect row stamped month t holds the relative importance BLS
#    labels month t-1. The June 2026 tables print "Relative importance May 2026",
#    and that column is the 2026-06-01 aspect rows -- all 307 items exact. The
#    2026-05-01 rows match 43. So `weight` is used as-is for a 1-month
#    contribution; lagging it is the bug.
#
# 2. CALENDAR LAGS. BLS omits rows entirely for intermittently priced items
#    rather than writing NA. Computing percent changes with a positional lag()
#    misses eight items in the June 2026 table -- Photographers and photo
#    processing at -1.1 against a published +1.9, Motor oil at -3.2 against -1.0,
#    Admission to sporting events at +4.6 against +6.2. Joining on a shifted date
#    gets all 371.
# ==============================================================================

library(tidyverse)
library(rvest)
library(tidyusmacro)

BLS_UA <- "R CPI research (rortybomb@gmail.com)"

# The news release and cu.item disagree on the Oxford comma for two apparel
# items ("swimwear, and accessories" vs "swimwear and accessories"). Only the
# name join in this diagnostic cares -- production joins on item_code -- but
# normalizing keeps the unmatched list meaningful instead of permanently at two.
normalize_item_name <- function(x) str_squish(str_replace_all(x, ",\\s+and\\b", " and"))

# ------------------------------------------------------------- scrape a table
# The two tables have the same seven-column shape: item, relative importance,
# percent change, effect, standard error, "largest/smallest since" date, and the
# percent change at that reference month.
fetch_bls_table <- function(url) {
  html <- rvest::read_html(httr::GET(url, httr::user_agent(BLS_UA)))

  caption <- html %>% rvest::html_element("table caption") %>% rvest::html_text2()
  ref <- str_match(caption, "([A-Z][a-z]+ \\d{4})")[, 2]

  rows <- html %>%
    rvest::html_element("table") %>%
    rvest::html_elements("tr") %>%
    map(~ rvest::html_elements(.x, "th, td") %>% rvest::html_text2() %>% str_squish())

  rows <- keep(rows, ~ length(.x) == 7)

  tibble(
    raw_name = map_chr(rows, 1),
    ri       = map_chr(rows, 2),
    pct      = map_chr(rows, 3),
    eff      = map_chr(rows, 4),
    se       = map_chr(rows, 5),
    since    = map_chr(rows, 6)
  ) %>%
    mutate(
      # Footnote 4 in Table 6 marks items published only unadjusted; those rows
      # show an NSA percent change even though the rest of the column is SA.
      nsa_only  = str_detect(raw_name, "\\(4\\)"),
      item_name = normalize_item_name(str_remove_all(raw_name, "\\(\\d+\\)")),
      across(c(ri, pct, eff, se), ~ suppressWarnings(as.numeric(.x))),
      ref_month = ref
    ) %>%
    select(-raw_name)
}

# ---------------------------------------------------- percent changes, our side
# Calendar-joined, never positional. See note 2 in the header.
cpi_percent_changes <- function(cpi_data, target_date) {
  base <- cpi_data %>%
    filter(
      area_code == "0000", periodicity_code == "R",
      str_starts(period, "M"), period != "M13", !is.na(value)
    ) %>%
    select(seasonal, item_code, item_name, date, value)

  shifted <- function(k, nm) {
    base %>%
      mutate(date = date %m+% months(k)) %>%
      select(seasonal, item_code, date, !!nm := value)
  }

  out <- base %>%
    left_join(shifted(1, "v_l1"), by = c("seasonal", "item_code", "date")) %>%
    left_join(shifted(12, "v_l12"), by = c("seasonal", "item_code", "date")) %>%
    filter(date == target_date) %>%
    mutate(
      pct1  = (value / v_l1 - 1) * 100,
      pct12 = (value / v_l12 - 1) * 100
    )

  list(
    sa  = out %>% filter(seasonal == "S") %>% select(item_code, item_name, pct1_sa = pct1),
    nsa = out %>% filter(seasonal == "U") %>%
      select(item_code, item_name, pct1_nsa = pct1, pct12_nsa = pct12)
  )
}

# --------------------------------------------------------------- the comparison
tally <- function(label, mine, theirs, tol = 1e-9) {
  ok <- !is.na(mine) & !is.na(theirs)
  tibble(
    check = label,
    n = sum(ok),
    exact = sum(abs(mine[ok] - theirs[ok]) < tol),
    max_dev = if (any(ok)) max(abs(mine[ok] - theirs[ok])) else NA_real_
  )
}

replicate_bls_tables <- function(cpi_data,
                                 target_date = NULL,
                                 verbose = TRUE) {
  if (is.null(target_date)) {
    target_date <- max(cpi_data$date[!is.na(cpi_data$value)], na.rm = TRUE)
  }

  t6 <- fetch_bls_table("https://www.bls.gov/news.release/cpi.t06.htm")
  t7 <- fetch_bls_table("https://www.bls.gov/news.release/cpi.t07.htm")

  # The scraped tables describe one month. If the pipeline is on a different
  # month the comparison is meaningless, so stop rather than report noise.
  scraped <- as.Date(paste("01", t6$ref_month[1]), format = "%d %B %Y")
  if (!identical(scraped, target_date)) {
    stop(
      "Table 6 on bls.gov is for ", format(scraped, "%B %Y"),
      " but the pipeline's latest month is ", format(target_date, "%B %Y"),
      ". Re-run scripts/01_download_cpi_data.R, or pass target_date explicitly."
    )
  }

  lut <- cpi_data %>%
    filter(area_code == "0000") %>%
    distinct(item_code, item_name) %>%
    mutate(item_name = normalize_item_name(item_name))
  pc <- cpi_percent_changes(cpi_data, target_date)

  # Weights and effects as the pipeline hands them over. Relative importance is
  # item-level and rides on both the SA and NSA rows, so take it across both --
  # roughly 37 items in Table 6 are published unadjusted only and have no SA row
  # at all. The effect columns are adjustment-specific and stay separate.
  ours <- cpi_data %>%
    filter(area_code == "0000", periodicity_code == "R", date == target_date) %>%
    distinct(item_code, weight, weight_12mo) %>%
    filter(!is.na(weight))
  ours_sa <- cpi_data %>%
    filter(
      area_code == "0000", periodicity_code == "R",
      date == target_date, seasonal == "S"
    ) %>%
    select(item_code, effect_1m)
  ours_nsa <- cpi_data %>%
    filter(
      area_code == "0000", periodicity_code == "R",
      date == target_date, seasonal == "U"
    ) %>%
    select(item_code, effect_12m)

  # Standard errors are not carried on cpi_data; pull them straight from the
  # aspect file so Tables 6 and 7 are checked end to end rather than in part.
  asp <- getCPIAspects("rortybomb@gmail.com", aspect_type = c("M1", "MC"))
  ses <- asp %>%
    filter(area_code == "0000", date == target_date) %>%
    select(item_code, aspect_type, value_num) %>%
    pivot_wider(names_from = aspect_type, values_from = value_num)

  j6 <- t6 %>%
    inner_join(lut, by = "item_name") %>%
    left_join(ours, by = "item_code") %>%
    left_join(ours_sa, by = "item_code") %>%
    left_join(pc$sa %>% select(-item_name), by = "item_code") %>%
    left_join(pc$nsa %>% select(-item_name), by = "item_code") %>%
    left_join(ses, by = "item_code") %>%
    mutate(my_pct = if_else(nsa_only | is.na(pct1_sa), pct1_nsa, pct1_sa))

  j7 <- t7 %>%
    inner_join(lut, by = "item_name") %>%
    left_join(ours, by = "item_code") %>%
    left_join(ours_nsa, by = "item_code") %>%
    left_join(pc$nsa %>% select(-item_name), by = "item_code") %>%
    left_join(ses, by = "item_code")

  results <- bind_rows(
    tally("T6 relative importance", j6$weight, j6$ri),
    tally("T7 relative importance", j7$weight, j7$ri),
    tally("T6 1-month percent change", round(j6$my_pct, 1), j6$pct),
    tally("T7 12-month percent change", round(j7$pct12_nsa, 1), j7$pct),
    tally("T6 effect on all items", j6$effect_1m, j6$eff),
    tally("T7 effect on all items", j7$effect_12m, j7$eff),
    tally("T6 median standard error", j6$M1, j6$se),
    tally("T7 median standard error", j7$MC, j7$se)
  ) %>%
    mutate(pass = n > 0 & exact == n)

  if (verbose) {
    cat("\nBLS Tables 6 and 7,", format(target_date, "%B %Y"), "\n")
    print(results, n = 20)

    misses <- bind_rows(
      j6 %>% filter(!is.na(weight), abs(weight - ri) > 1e-9) %>%
        transmute(table = "T6", check = "relative importance", item_name,
                  published = ri, ours = weight),
      j6 %>% filter(!is.na(my_pct), abs(round(my_pct, 1) - pct) > 1e-9) %>%
        transmute(table = "T6", check = "1-month pct", item_name,
                  published = pct, ours = round(my_pct, 1)),
      j7 %>% filter(!is.na(pct12_nsa), abs(round(pct12_nsa, 1) - pct) > 1e-9) %>%
        transmute(table = "T7", check = "12-month pct", item_name,
                  published = pct, ours = round(pct12_nsa, 1)),
      j6 %>% filter(!is.na(effect_1m), abs(effect_1m - eff) > 1e-9) %>%
        transmute(table = "T6", check = "effect", item_name,
                  published = eff, ours = effect_1m),
      j7 %>% filter(!is.na(effect_12m), abs(effect_12m - eff) > 1e-9) %>%
        transmute(table = "T7", check = "effect", item_name,
                  published = eff, ours = effect_12m)
    )
    if (nrow(misses) > 0) {
      cat("\nMismatches:\n")
      print(misses, n = 50)
    } else {
      cat("\nNo mismatches on any matched item.\n")
    }

    # Unmatched rows are not failures -- BLS prints some special indexes with no
    # relative importance, and a handful of item names differ between the news
    # release and cu.item. Worth surfacing so the coverage is never assumed.
    unmatched <- setdiff(t6$item_name, lut$item_name)
    cat("\nTable 6 rows with no item_code match:", length(unmatched), "\n")
    if (length(unmatched) > 0) cat(paste0("  ", unmatched, collapse = "\n"), "\n")
  }

  invisible(list(summary = results, table6 = j6, table7 = j7))
}
