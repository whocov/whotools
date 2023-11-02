#' A general table theme for the `{gt}` packag
#'
#' @param x a gt object
#'
#' @import gt
#'
#' @author HLS
#'
#' @export
#'
theme_who_gt <- function(x, mode = c("light", "dark")) {

  mode <- match.arg(mode)
  mode <- mode[1]

  header_fill <- dplyr::case_when(mode == "light" ~ "#eff3ff",
                                  mode == "dark" ~ "#00205C")

  header_text <- dplyr::case_when(mode == "light" ~ "black",
                                  mode == "dark" ~ "white")

  x %>%
    tab_style(
      style = list(
        cell_borders(sides = "top", color = "grey")
      ),
      locations = cells_body()
    ) %>%
    tab_style(
      style = list(
        cell_borders(sides = "left", color = "lightgrey")
      ),
      locations = cells_body(columns = 2:last_col())
    ) %>%
    tab_style(
      style = list(
        cell_text(align = "left")
      ),
      locations = cells_stub(rows = TRUE)
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold", align = "center", v_align = "middle", color = header_text),
        cell_fill(color = header_fill)
      ),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold", align = "center", v_align = "middle", color = header_text),
        cell_fill(color = header_fill)
      ),
      locations = cells_column_spanners()
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_fill(color = "#eff3ff")
      ),
      locations = cells_stubhead()
    )

}
