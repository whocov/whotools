
#' This function extends `cut` functionality to allow for several labelling
#' styles
#'
#' @param x a `numeric` vector containing numbers to be binned
#'
#' @param breaks a numeric vector of breaks to be used for cut. There is no need
#'   to add Inf.  For percentages, provide numbers as proportions.
#'
#' @param mode one of "int" (a vector of integer values, e.g. age, cases), "dbl"
#'   (a vector of floating point numbers with decimals e.g. attack rate), "pct"
#'   (a vector of percentages, expressed as decimals), or "pct_change" (a vector
#'   of percentages, specifically when showing percent change between two time
#'   points).
#'
#'
#' @author HLS
#'
#' @export
#'
bin_numbers <- function(x,
                        breaks = c(0, 5, 18, 45, 65),
                        mode = c("int", "dbl", "pct", "pct_change")) {

  mode <- match.arg(mode)

  breaks <- c(breaks, Inf)

  switch(
    mode,
    "int" = {labs <- int_labs(breaks = breaks)},
    "dbl" = {labs <- dbl_labs(breaks = breaks)},
    "pct" = {labs <- pct_labs(breaks = breaks)},
    "pct_change" = {breaks <- c(-Inf, breaks); labs <- pct_change_labs(breaks = breaks)}
  )

  cut(x, breaks = breaks, labels = labs,
      include.lowest = TRUE,
      right = ifelse(mode == "int", FALSE, TRUE)
      )

}


#' @noRd
int_labs <- function(breaks, sep = "-") {

  v1 <- paste0(breaks)
  v2 <- paste0(dplyr::lead(breaks) - 1)
  v2[v1 == v2] <- NA

  paste(v1, v2, sep = sep) %>%
    stringr::str_replace(paste0(sep, NA), "") %>%
    stringr::str_replace(paste0(sep, Inf), "+") %>%
    .[-length(.)]
}



#' @noRd
dbl_labs <- function(breaks, sep = "-") {

  v2 <- dplyr::lead(breaks)
  v2[breaks == v2] <- NA

  paste(paste0(">", breaks), v2, sep = sep) %>%
    stringr::str_replace(paste0(sep, "(NA|Inf)"), "") %>%
    stringr::str_replace(">0", "0") %>%
    .[-length(.)]
}


#' @noRd
pct_labs <- function(breaks, sep = "-") {

  breaks <- breaks * 100
  v2 <- dplyr::lead(breaks)
  v2[breaks == v2] <- NA

  paste(paste0(">", breaks), paste0(v2, "%"), sep = sep) %>%
    stringr::str_replace(paste0(sep, "(NA|Inf)"), "") %>%
    stringr::str_replace(">0", "0") %>%
    .[-length(.)]
}



#' @noRd
pct_change_labs <- function(breaks, sep = "-") {

  v1 <- breaks * 100
  v2 <- dplyr::lead(v1)

  dplyr::case_when(
    v1 == 0 & v2 == 0 ~ stringr::str_glue("No change (0%)"),
    is.infinite(v1) & v1 < 0 ~ stringr::str_glue("Decreasing by {abs(v2)}+%"),
    v1 < 0 & v2 <= 0 ~ stringr::str_glue("Decreasing by {abs(v2)}-{abs(v1)}%"),
    v1 < 0 & v2 > 0 ~ stringr::str_glue("Limited change ({abs(v1)}-{abs(v2)}%)"),
    is.infinite(v2) & v2 > 0 ~ stringr::str_glue("Increasing by {v1}+%"),
    v1 >= 0 & v2 > 0 ~ stringr::str_glue("Increasing by {v1}-{v2}%")
  ) %>%
    .[-length(.)]

}

