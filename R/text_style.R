
#' This function creates HTML text styling via inputted css,, for use in rmarkdown documents
#'
#' @param text the text within an rmarkdown document to be stylised
#'
#' @param size the size of the text using css conventions.
#' Use a number followed by 'px' (pixels) or 'em' (relative measure to current font size)
#'
#' @param col the colour of the font using css conventions.
#' Use a standard HTML colour name ("red", "blue", "grey"), rgb colour ("rgb(255, 99, 71)"),
#' or a hex colour ("#e41a1c")
#'
#' @param weight the weight of the font using css conventions.
#' Use an number value (normal is 100), or a css naming convention (e.g. "light", "normal", "bold")
#'
#' @param style the style of the font using css conventions.
#' Use a css naming convention (e.g. "normal", "italic")
#'
#' @param ... any additional css arguments to apply to the text, provided in css style
#' e.g. ('font-family: "Times New Roman"'). These can be separated by ';', or provided as a vector.
#'
#'
#' @author HLS
#'
#' @export
#'


text_style <- function(text, size = "1em", col = "black", weight = "normal", style = "normal", ...) {


  stringr::str_glue("<span style='color: {col}; font-size: {size};\\
                    font-weight: {weight}; font-style: {style};\\
                    {paste0(..., collapse = ';')}'>{text}</span>")

}
