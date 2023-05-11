#' This function uses `binom.test` to derive estimates of the 95% CI for
#' proportions.
#'
#' @param k number of successes
#'
#' @param n number of trials
#'
#' @param conf confidence level, defaults to 0.95
#'
#' @param result a `character` vector indicating which bounds of the confidence
#'   interval to return: can be `"lower"`, or `"upper"`, or `"both"`
#'
#' @author Thibaut Jombart, Finlay Campbell, Henry LS
#'
#' @export
#'
ci_proportion <- function(k, n,
                          conf = 0.95,
                          result = c("both", "lower", "upper")
) {

  result <- match.arg(result)
  if (result == "both") result <- c("lower", "upper")



  ci_prop <- function(k, n, conf, result) {
    if (is.na(n)) return(NA_integer_)

    if (n == 0) {
      out <- c(0, 1)
    } else {
      out <- binom.test(k, n, conf.level = conf)$conf.int
    }
    names(out) <- c("lower", "upper")
    return(out[result])
  }

  if (length(result) > 1) {
    purrr::map2(k, n, ci_prop, conf = conf, result = result)
  } else {
    purrr::map2_dbl(k, n, ci_prop, conf = conf, result = result)
  }

}
