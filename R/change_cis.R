#' Make a y axis for an epicurve, prepackaged with defaults
#'
#' @param new_val The integer value more recently occurring in time
#'
#' @param old_val The integer value less recently occurring in time
#'
#' @param dist the assumed distribution the two values are taken from. choose from "poisson" and "negbin"
#'
#' @param output should the output be via the confidence interval of
#' the ratio between the two values or the percentage change. choose from
#' "pct_change" or "ratio"
#'
#' @param expand the expand argument to `scale_y_continuous()`. Defaults to `expansion(mult = c(0, NA))`
#'
#' @param conf confidence level, defaults to 0.95
#'
#' @param theta theta parameter, for negative binomial distribution only
#'
#' @author Henry LS, Finlay Campbell
#'
#' @export



ci_change <- function(new_val,
                      old_val,
                      dist = c("poisson", "negbin"),
                      output = c("pct_change", "ratio"),
                      conf = 0.95,
                      theta) {

  dist <- match.arg(dist)
  output <- match.arg(output)

  if (dist == "negbin" & missing("theta")) stop("must supply theta for negbin model")

  in_df <- data.frame(x = c(old_val, new_val), time = c(0, 1))

  run_mod <- function(nv, ov) {
    if (NA %in% c(nv, ov)) return(NA_real_)
    if (0 %in% c(nv, ov)) return(NA_real_)

    df <- data.frame(x = c(ov, nv), time = c(0, 1))
    if (dist == "poisson") {
      glm(x ~ time, data = df, family = "poisson")
    } else {
      MASS::glm.nb(x ~ time, data = df, init.theta = theta)
    }
  }

  mod <- purrr::map2(old_val, new_val, run_mod)

  out <- purrr::map(mod, ~confint(.x, level = conf)["time",])

  if (output == "pct_change") out <- purrr::map(out, ~sort(exp(.x) - 1))

  out <- purrr::map(out, ~purrr::set_names(.x, c("lower", "upper")))

  return(out)
}

