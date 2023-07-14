#' Make a y axis for an epicurve, prepackaged with defaults
#'
#' @param mode one of "cases" or "incidence" - if in case mode, only integer values will be shown in labels,
#'    otherwise, there is potential to show decimal points
#'
#' @param limits The limits argument to `scale_y_contiunous()`.
#'    Y-axis limits for the epicurve - by default this is `c(0, NA)` - i.e. default for the upper limit,
#'    and 0 as the lower limit
#'
#' @param lower_maximum The lowest possible upper limit, as a numeric value. This will mean the upper y axis limit, for all facets will by this
#' number at minimum. Useful when dealing with very low numbers, defaults to 5.
#'
#' @param expand the expand argument to `scale_y_continuous()`. Defaults to `expansion(mult = c(0, NA))`
#'
#' @author Henry LS
#'#'
#'
#' @import ggplot2
#'
#' @export


scale_y_epicurve <- function(mode = c("cases", "incidence"),
                             limits = c(0, NA),
                             lower_maximum = 5,
                             expand = expansion(mult = c(0, 0.05)),
                             ...) {

  mode <- match.arg(mode)

  if (mode == "cases") acc <- 1 else acc <- NULL

  list(
    expand_limits(y = lower_maximum),
    scale_y_continuous(
      expand = expand,
      breaks = scales::pretty_breaks(),
      labels = scales::comma_format(accuracy = acc),
      limits = limits,
      ...
    )
  )

}
