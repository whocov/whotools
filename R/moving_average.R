#' function to apply a moving average to a numeric column. Use this function
#' within `mutate()` Note that function requires data to be sorted by date. By
#' default calculates a 7 day moving average.
#'
#' @param val - which `numeric` class variable to be used
#' @param date_val - which `date` class index to be used
#' @param before - the number of days to include before index. Defaults to 3.
#' @param after - the number of days to include after index. Defaults to 3.
#'
#' @importFrom slider slide_index_dbl

#' @export
#'
moving_average <- function(val, date_val, before = 3, after = 3) {
  slider::slide_index_dbl(
    .x = as.double(val),
    .i = as.Date(date_val),
    .before = before,
    .after = after,
    .f = ~mean(., na.rm = T)
  )
}
