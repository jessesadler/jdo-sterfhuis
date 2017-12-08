## Function to convert running accounts to xts and from xts ##

library(tidyr)
library(timetk)
library(xts)

# To xts takes a running account tbl with date, id/group, and current value columns
# It uses tidyr to widen the tbl, timetk to convert to xts object,
# and the na.locf function to fill in amounts for each day
# Returns an xts function with one observation for each id for each day

to_fill_xts <- function(df, id = id, current = current) {
  df_wide <- df %>% spread(id, current)
  df_xts <- tk_xts(df_wide)
  na.locf(merge(df_xts, seq(min(df$date),
                                  max(df$date), by = 1)))
}

# From xts to tbl
# Reverses `to_fill_xts`, but with value for each day for each id/group
# Only needs xts object with dates as row names, ids as column names, and values
# Result is a tibble with date, id, and current variables

from_fill_xts <- function(xts) {
  tbl <- tk_tbl(xts, rename_index = "date")
  tbl %>% gather(id, current, -date)
}