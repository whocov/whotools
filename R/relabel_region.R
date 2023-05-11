#' Change region labels from long to short form and vice versa.
#'
#' @param region A character vector of regions to relabel.
#'
#' @author Finlay Campbell, Henry LS, Steve Kerr
#'
#' @export
#'
relabel_region <- function(region) {

  labels <- c(
    AFR = "African Region",
    EUR = "European Region",
    EMR = "Eastern Mediterranean Region",
    PAH = "Region of the Americas",
    AMR = "Region of the Americas",
    SEAR = "South-East Asia Region",
    WPR = "Western Pacific Region",
    OTHER = "Other"
  )
  labels <- c(labels, setNames(names(labels), labels))

  regions <- gsub("O$", "", region)
  change <- regions %in% names(labels)
  region[change] <- labels[regions[change]]

  return(region)

}
