#' @export
plot_final_series_dygraph <- function(z, main) {
  z %>%
    ts_pick(c("adj", "orig")) %>%
    ts_dygraphs(main = main)
}

