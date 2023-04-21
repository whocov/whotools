#' This function visualizes epidemic curves based on aggregated case or death counts.
#'
#' @param dat A data frame containing number of cases/deaths per day to be visualized.
#' @param start_date The start date of the visualization. Defaults to NULL, which means that the earliest date in the data will be used.
#' @param end_date The end date of the visualization. Defaults to `Sys.Date()`, which means that the current date will be used.
#' @param var A variable to be used for the y-axis. Defaults to `new_cases`.
#' @param date_var A variable to be used for the x-axis. Defaults to `date_reported`.
#' @param by_week A logical value indicating whether to aggregate the data by week. Defaults to FALSE.
#' @param plot_title The plot title. Defaults to NULL, which means that no title will be used.
#' @param fill_by The variable to be used for the fill aesthetic. Defaults to "who_region_long".
#' @param facet_by The variable to be used for faceting. Defaults to NULL, which means that no faceting will occur.
#' @param free_y A logical value indicating whether the y-axis should be scaled independently for each facet. Defaults to TRUE.
#' @param col_pal The color palette to be used. Defaults to `outbreak_pal("qual1")`.
#' @param facet_cols The number of columns in the facet grid. Defaults to 2.
#' @param return_data A logical value indicating whether to return the data once processed. Defaults to FALSE.
#' 
#' @return If return_data is TRUE, returns a data frame. Otherwise, it returns a ggplot object.
#' 
#' @author Henry Laurenson-Schafer, Steve Kerr
#' 
#' @export

vis_agg_epicurve <- function(
    dat,
    start_date = NULL, 
    end_date = Sys.Date(), 
    var = new_cases, ### UPDATE, as needed
    date_var = date_reported, ### UPDATE, as needed
    by_week = FALSE,
    plot_title = NULL,
    fill_by = "who_region_long", 
    facet_by = NULL,
    free_y = TRUE,
    col_pal = outbreak_pal("qual1"),
    facet_cols = 2,
    return_data = FALSE) 
{
  
  if (is.null(dat)) return(NULL)
  if (!is.null(facet_by)) {
    if (facet_by == "none") facet_by <- NULL
  }
  
  
  if (is.null(start_date)) {
    start_date <- min(dat %>% filter({{ var }} != 0) %>%  pull({{ date_var }}))
  }
  
  
  dat <- dat %>%
    mutate(who_region_long = long_whoreg(who_region)) %>%
    filter(between({{ date_var }}, start_date, end_date)) %>%
    ungroup()


  if (nrow(dat) == 0) return(NULL)
  
  
  dat <- dat %>% 
    group_by(who_region, {{ date_var }}, who_region_long, across(any_of(fill_by)), across(any_of(facet_by))) %>% 
    summarise(across(contains("new"), \(x) sum(x, na.rm = TRUE)),
              across(contains("total"), \(x) max(x, na.rm = TRUE))) %>% 
    ungroup()
  
  
  if (by_week) {
    dat <- dat %>%
      mutate({{ date_var }} := floor_date({{ date_var }}, "week", week_start = 1)) %>% 
      group_by(who_region, {{ date_var }}, who_region_long, across(any_of(fill_by)), across(any_of(facet_by))) %>% 
      summarise(across(contains("new"), \(x) sum(x, na.rm = TRUE)),
                across(contains("total"), \(x) max(x, na.rm = TRUE))) %>% 
      ungroup()
  }
  
  
  if (return_data) return(dat)
  
  
  var_type <- dat %>% select({{ var }}) %>% names()
  
  
  dat <- dat %>%
    rename(var = {{ var }}, date = {{ date_var }})

  
  xmin <- max(start_date, min(dat$date))
  
  
  lower_exp <- 0.05
  if (start_date > xmin) {
    xmin <- start_date - 1
    lower_exp <- c(0, 0.05)
  }
  
  
  date_range = max(dat$date) - xmin
  
  
  if (date_range >= 6) {
    dbs <- paste0(floor(date_range / 6), " days")
  } else {
    dbs <- "1 day"
  }
  
  
  ylims <- c(0, NA)
  
  
  max_n <- max(dat$var, na.rm = TRUE)
  if (max_n < 5) {
    ylims = c(0, 5)
  }
  
  
  out <- ggplot(dat, aes(x = date, y = var, fill = .data[[fill_by]])) +
    geom_col(
      col = ifelse(by_week, "black", NA),
      linewidth = 0.5,
      width = ifelse(by_week, 7, 1)
      ) + 
    scale_x_date(
      date_labels = "%d %b\n%Y",
      date_breaks = dbs,
      limits = c(xmin, NA), expand = expansion(mult = lower_exp)) +
    scale_y_continuous(
      expand = expansion(mult = c(0.02, NA)),
      labels = scales::comma_format(),
      limits = ylims
      ) +
    scale_fill_manual(values = col_pal) +
    expand_limits(y = 5) +
    labs(
      x = ifelse(by_week, "Week of reporting", "Date of reporting"),
      y = glue::glue("Number of {ifelse(str_detect(var_type, 'death'), 'deaths', 'cases')}"), ### UPDATE, as needed
      fill = NULL,
      title = plot_title,
      subtitle = str_glue("data as of {format(max(dat$date), '%d %b %Y')}"),
      caption = "Source: WHO"
    ) +
    theme_outbreak() +
    theme(legend.position = "bottom", plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm"))
  
  
  if (!is.null(facet_by)) {
    out <- out +
      facet_wrap(
        as.formula(paste0("~", facet_by)),
        scales = ifelse(free_y, "free_y", "fixed"),
        ncol = facet_cols
        )
  }
  
  
  if (fill_by == "who_region_long") {
    out <- out +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  }
  
  out 

}
