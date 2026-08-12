# This file downloads the CPI files from BLS.gov, formats them, and merges them.
#
# Weights come from the BLS aspect flat file (cu.aspect) via tidyusmacro, not
# from weights/inflation_weights.csv. That CSV was a hand-maintained annual
# December table applied as a step function; the aspect file is monthly, keyed on
# item code rather than item name, and restamped with every CPI release.
#
# Columns getBLSFiles() attaches when weights = TRUE:
#   weight       relative importance on the base month for the 1-month change
#                ending in this observation month. Do NOT lag it -- a cu.aspect
#                row stamped month t already holds the RI BLS labels month t-1.
#   weight_12mo  same, for the 12-month change ending in this month.
#   effect_1m    BLS's own published contribution to the 1-month all items
#                change, percentage points. Seasonally adjusted rows only.
#   effect_12m   same, 12-month, not seasonally adjusted rows only.
#
# effect_1m and effect_12m reproduce news release Tables 6 and 7 exactly. Prefer
# them to computing contributions by hand; see scripts/01c_replicate_bls_tables.R
# for the check that establishes this and re-runs on any release.
#
# Coverage: U.S. city average (area_code "0000"), March 2012 forward. Outside
# that window every weight column is NA rather than back-filled.

# Libraries
library(tidyusmacro)

cpi_data <- getBLSFiles("cpi", "rortybomb@gmail.com")

# ---------------------------------------------------------------- sanity checks
# Release mode: fail loudly here rather than let a silently empty weight column
# propagate into every contribution chart downstream.
local({
  stopifnot(all(c("weight", "weight_12mo", "effect_1m", "effect_12m") %in% names(cpi_data)))

  latest <- max(cpi_data$date[!is.na(cpi_data$value)], na.rm = TRUE)

  all_items <- cpi_data %>%
    dplyr::filter(
      item_code == "SA0", area_code == "0000",
      seasonal == "S", periodicity_code == "R", date == latest
    )
  if (nrow(all_items) != 1) {
    stop("Expected exactly one All items SA row at ", latest, ", got ", nrow(all_items))
  }
  # Relative importance is defined as a share of all items, so this is 100 by
  # construction. If it is not, the join key is wrong.
  if (!isTRUE(all.equal(all_items$weight, 100))) {
    stop("All items weight is ", all_items$weight, " at ", latest, ", expected 100")
  }

  covered <- cpi_data %>%
    dplyr::filter(area_code == "0000", date == latest, seasonal == "S", !is.na(value))
  message(
    "CPI through ", format(latest, "%B %Y"), ": ",
    sum(!is.na(covered$weight)), " of ", nrow(covered),
    " U.S. city average SA series carry a weight; ",
    sum(!is.na(covered$effect_1m)), " carry a published 1-month effect."
  )
})
