
#' @title ref_country
#' @description a reference table of all UN countries, alongside WHO region, and iso2 and 3 codes.
#' @format A tibble with 238 rows and 4 variables:
#' \describe{
#'   \item{\code{country}}{character the UN name of the country}
#'   \item{\code{who_region}}{character the WHO region of the country}
#'   \item{\code{iso2}}{character The ISO2 code for the country}
#'   \item{\code{iso3}}{character The ISO3 code for the country}
#'}
"ref_country"


#' @title country_synonyms
#' @description synonym table of common country synoyms that are *NOT* WHO official
#' @format A tibble with 39 rows and 2 variables:
#' \describe{
#'   \item{\code{from}}{character the non-official synonym}
#'   \item{\code{to_name}}{character the official country name}
#'   \item{\code{to_iso3}}{character the iso3 code of the country}

#'}
"country_synonyms"
