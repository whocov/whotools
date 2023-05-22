#' a general ggplot2 theme for use across products
#'
#' @param base_size the base size of text in the plot
#'
#' @param ... additional theme arguments
#'
#' @import ggplot2
#'
#' @author HLS
#'
#' @export
#'
theme_who <- function(base_size = 11, ...) {

  theme_minimal(base_size) +
    theme(

      axis.line.x.bottom = element_line(color = "black"),

      ## axis title size and face
      axis.title = element_text(size  = 12, face  = "bold"),

      legend.text = element_text(size=12),

      ## remove the grey background in legend
      legend.key = element_blank(),

      ## remove y -axis lines
      axis.line.y =  element_blank(),

      # background
      panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", colour = NA),

      # caption (italics, on left side)
      plot.caption = element_text(hjust = 0, size = 10),

      ## facet appearance
      strip.text.x = element_text(size=12, face = "bold"),
      strip.text.y = element_text(size=12, face="bold", hjust =  0),

      ...
    )

}
