#' This function exports a range of predefined palettes
#'
#' @param type a `character` name of a palette to be exported. Choose from "qual1", "qual2", "qual3", "qual4", "binary1", "binary2",
#' "who", "region", "change1", or "change2"
#'
#' @param labels optional - a `character` vector of labels to apply to each colour.
#'
#' @param n optional - the number of colours to output. By default, the maximum in the palette is used.
#' Most useful for change palettes, where the difference between colour grades will increase as n decreases.
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @author HLS
#'
#' @export
#'
who_palette <- function(type = c("qual1", "qual2", "qual3", "qual4", "binary1", "binary2",
                                 "who", "region", "change1", "change2"),
                         labels = NULL,
                         n = Inf) {

  type <- match.arg(type)

  if (length(labels) > n) n <- length(labels)

  res <- switch(
    type,
    "qual1" =  c('#8dd3c7','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9', '#ffffb3'),
    "qual2" = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999'),
    "qual3" = c('#fbb4ae','#b3cde3','#ccebc5','#decbe4','#fed9a6','#ffffcc','#e5d8bd','#fddaec','#f2f2f2'),
    "qual4" = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5',
                '#d9d9d9','#bc80bd','#ccebc5','#ffed6f', 'black', 'white'),
    "who" = c("#0093D5", "#D86422", "grey"),
    "binary1" = c("#2166ac", "#b2182b", "grey"),
    "binary2" = c("#e08214", "#542788", "grey"),
    "region" = c("AFRO"   = "#66c2a5",
                 "PAHO"   = "#fc8d62",
                 "EMRO"   =  "#e78ac3",
                 "EURO"   = "#8da0cb",
                 "SEARO"  = "#a6d854",
                 "WPRO"   = "#e5c494",
                 "OTHER"  = "lightgrey"),
    "change1" = suppressWarnings(RColorBrewer::brewer.pal(n, "RdYlBu")),
    "change2" = suppressWarnings(RColorBrewer::brewer.pal(n, "RdYlGn"))
  )

  if (!is.null(labels)) {
    res <- res[length(labels)]
    names(res) <- labels
  }

  return(res)

}
