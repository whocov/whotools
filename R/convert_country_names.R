#' function to convert country names via a synonym table, using a standard to/from structure
#'
#' @param x - the dataframe where conversion takes place
#' @param country_col - a character object with the name of the column in your dataset corresponding to country.
#'      Defaults to "country"
#' @param show_corrections - a logical. Should a logical column (named is_corrected) be additionally outputted, showing
#'      where corrections have taken place?
#' @param ref a data frame. A reference synonym dataframe, usually with a "to" and "from" column. Defaults to the prepackaged country_synonyms
#' @param ref_from_col - a character object with the name of the column with non-official synonyms in the reference dataset
#' @param from_to_col - a character object with the name of the column with official country names in the reference dataset
#'
#' @author Henry LS

#' @export



convert_country_names <- function(x,
                                  country_col = "country",
                                  show_corrections = TRUE,
                                  ref = country_synonyms,
                                  ref_from_col = "from",
                                  ref_to_col = "to") {

  match.arg(country_col, choices = colnames(x))
  match.arg(ref_from_col, choices = colnames(ref))
  match.arg(ref_to_col, choices = colnames(ref))

  ref <- ref %>%
    dplyr::mutate(from_upper = stringr::str_to_upper(.data[[ref_from_col]])) %>%
    dplyr::select(from_upper, any_of(ref_to_col))

  x %>%
    dplyr::mutate(cc_upper = stringr::str_to_upper(.data[[country_col]])) %>%
    dplyr::left_join(ref, by = c("cc_upper" = "from_upper")) %>%
    {
      if (show_corrections) {
        {.} %>%
          dplyr::mutate(is_corrected = ifelse(is.na(.data[[ref_to_col]]), FALSE, TRUE))
      } else {
        {.}
      }
    } %>%
    dplyr::mutate("{country_col}" := dplyr::coalesce(.data[[ref_to_col]], .data[[country_col]])) %>%
    dplyr::select(-any_of(ref_to_col), -cc_upper)


}
