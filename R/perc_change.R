#' This function calculates percentage change between two values (i.e. for week on week change)
#'
#' @param new_val the value more recent in time
#'
#' @param old_val the value further back in time
#'
#' @author HLS
#'
#' @export



perc_change <- function(new_val, old_val)

{
  change <- case_when(
    is.na(new_val) | new_val == 0 & (is.na(old_val) | old_val == 0) ~ NA_real_,
    old_val > 0 & new_val == 0 ~ NA_real_,
    new_val > 0 & old_val == 0 ~ NA_real_,
    new_val < 0 & old_val < 0 ~ 0,
    old_val < 0 ~ 1,
    new_val < 0 ~ -1,
    TRUE ~ new_val/old_val - 1
  )

  return(change)
}
