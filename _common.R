set.seed(1014)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  fig.width = 6,
  fig.asp = 2/3,
  fig.show = "hold"
)

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  stringr.view_n = 10,
  # Activate crayon output - temporarily disabled for quarto
  crayon.enabled = TRUE,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

ggplot2::theme_set(ggplot2::theme_gray(12))

status <- function(type) {
  status <- switch(type,
    polishing = "should be readable but needs more polishing",
    restructuring = "is undergoing heavy restructuring and may be confusing or incomplete",
    drafting = "is currently a dumping ground for ideas, and we don't recommend reading it",
    complete = "is largely complete and just needs final proof reading",
    stop("Invalid `type`", call. = FALSE)
  )

  class <- switch(type,
    polishing = "note",
    restructuring = "important",
    drafting = "important",
    complete = "note"
  )

  cat(paste0(
    "\n",
    ":::: status\n",
    "::: callout-", class, " \n",
    "You are reading an early draft of *Seasonal Adjustment in R*. ",
    "This chapter ", status, ".\n",
    ":::\n",
    "::::\n"
  ))
}
