#' This function uses `binom.test` to derive estimates of confidence intervals
#' for proportions.
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
#' @param drop Drop dimensions if possible by coercing to vector
#'
#' @author Thibaut Jombart, Finlay Campbell, Henry LS
#'
#' @export
#'
ci_proportion <- function(k, n,
                          conf = 0.95,
                          result = c("both", "lower", "upper"),
                          drop = TRUE
                          ) {

  result <- match.arg(result)
  if (result == "both") result <- c("lower", "upper")

  get_ci <- function(k, n, conf, result) {
    if (is.na(n)) return(NA_integer_)
    if (n < 0 | k < 0 | k > n) return(NA_integer_)
    if (n == 0) out <- c(0, 1)
    else out <- binom.test(k, n, conf.level = conf)$conf.int
    names(out) <- c("lower", "upper")
    return(out[result])
  }

  out <- map2_dfr(k, n, get_ci, conf, result)

  if(drop && (nrow(out) == 1 || ncol(out) == 1)) out <- unlist(out)

  return(out)

}
