#' Change WHO region labels from long to short form and from region to office.
#'
#' @param label A character vector of regions in long or short form, as office
#'   or region.
#'
#' @param output Should the output be provided as a region ("region"), office
#'   ("office") or kept as initially specified ("keep").
#'
#' @param length Should the output be provided in abbreviated form ("short"), long
#'   form ("long") or switched ("switch"). Defaults to "switch".
#'
#' @author Finlay Campbell, Henry LS, Steve Kerr
#'
#' @examples
#' ## return the associated region as long text
#' relabel_region(
#'   c("EMR", "AFRO", "Regional Office for the Americas"),
#'   output = "region",
#'   length = "long"
#' )
#'
#' ## convert long format to short format and vice versa
#' relabel_region(c("EMR", "AFRO", "Regional Office for the Americas"))
#'
#' @export
#'
relabel_who <- function(label,
                        output = c("keep", "region", "office"),
                        length = c("switch", "short", "long")) {

  ## check arguments
  output <- match.arg(output)
  length <- match.arg(length)

  ## match region and office labels in short and long form
  index <- as.matrix(data.frame(
    region_short = c("AFR", "EUR", "EMR", "PAH", "AMR", "SEAR", "WPR"),
    region_long = c("African Region", "European Region",
                    "Eastern Mediterranean Region", "Region of the Americas",
                    "Region of the Americas", "South-East Asia Region",
                    "Western Pacific Region"),
    office_short = c("AFRO", "EURO", "EMRO", "PAHO", "AMRO", "SEARO", "WPRO"),
    office_long = c("Regional Office for Africa", "Regional Office for Europe",
                    "Regional Office for the Eastern Mediterranean",
                    "Regional Office for the Americas",
                    "Regional Office for the Americas",
                    "Regional Office for South-East Asia",
                    "Regional Office for the Western Pacific")
  ))

  ## get properties of label provided
  properties <- map_dfr(
    label,
    function(x) {
      mtch <- which(x == index, arr.ind = TRUE)
      if(length(mtch) == 0) data.frame(place = NA, region = NA, short = NA)
      else data.frame(
        ## row of the index indicating the place
        place = mtch[1,1],
        ## is it provided as a region (as opposed to office)
        region = mtch[1,2] %in% c(1, 2),
        ## is it provided in short output (as opposed to long)
        short = mtch[1,2] %in% c(1, 3)
      )
    }
  )

  ## change output according to arguments provided
  if(output == "region") properties$region <- TRUE
  else if(output == "office") properties$region <- FALSE
  if(length == "short") properties$short <- TRUE
  else if(length == "long") properties$short <- FALSE
  else if(length == "switch") properties$short <- !properties$short

  ## replace matching labels  with modified output
  label[!is.na(properties$place)] <- na.omit(index[matrix(
    c(properties$place, ifelse(properties$region, 1, 3) + !properties$short),
    ncol = 2
  )])

  return(label)

}
