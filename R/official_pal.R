#' This function exports a range of predefined palettes
#'
#' @importFrom magrittr `%>%`
#'
#' @param type a `character` name of a palette to be exported. Choose from "primary", "secondary", "ramp", "grey", "diverging","qualitative"
#'
#' @param subtype optional - a character defining a subtype of palette, for use in secondary, grey, and ramp palettes - this confines the palette
#' to a specific hue. Where no subtype has been defined, ramp and secondary palettes, will not be confined to a single hue.
#'
#' @param tint optional - an integer value between 10 and 100 defining the degree of tint applied to ramp or secondary palettes. Use this to obtain darker or lighter shades
#' of colours. Integer value snaps to nearest closest available tint. Tints greater than 100 are possible for ramp palettes.
#'
#' @param n optional - the number of colours to output. Here, the colours with the most spacing possible are chosen, while accounting for
#' colourblindness.
#'
#' @param use_names should the output vector have the colour names added? Defaults to FALSE.
#'
#' @author HLS
#'
#' @export
#'
who_palette_official <- function(
    type = c("primary", "secondary", "ramp", "grey", "diverging","qualitative"),
    subtype = NULL,
    tint = NULL,
    n = NULL,
    use_names = FALSE
) {

  type <- match.arg(type)


  x <- whotools::official_pal %>%
    dplyr::filter(type == {{ type }})


  if (!is.null(subtype)) {
    subtype <- match.arg(stringr::str_to_sentence(subtype), choices = unique(x$subtype), several.ok = TRUE)

    x <- x %>%
      dplyr::filter(subtype %in% {{ subtype }})


  }

  if (length(unique(x$subtype)) > 1) {
    if (is.null(tint)) {
      tint <- 100
    }
  }



  if (!is.null(tint)) {

    available_tints <- unique(x$tint)
    available_tints <- available_tints[!is.na(available_tints)]
    if (length(available_tints) != 0) {

      if (is.numeric(tint)) {
        tint <- available_tints[which.min(abs(available_tints - tint[1]))]

        x <- x %>%
          dplyr::filter(tint == {{ tint }})
      }

    }

  }

  if (!is.null(n)) {

    if (n == 1) {
      i <- dplyr::case_when(
        type %in% c("ramp", "diverging") ~ 3,
        TRUE ~ 1
      )
    } else if (n == 6 & type == "ramp") {
      i <- 1:6
    } else {

      max_n <- dplyr::case_when(
        type == "ramp" ~ 5,
        type == "grey" ~ 8,
        type == "secondary" ~ 4,
        type == "diverging" ~ 5
      )

      i <- quantile(1:max_n, floor(pal_quantiles(n)))
    }

    x <- x %>%
      dplyr::slice(i)

  }

  out <- x$hex

  if (use_names) {
    out <- rlang::set_names(out, x$name)
  }


  return(out)

}

#' @noRd
pal_quantiles <- function(n) {

  n <- 1:n
  (n - 1) / (max(n) - 1) +  n %% 1

}

