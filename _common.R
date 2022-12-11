set.seed(1014)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  # fig.width = 6,
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

# status("drafting", Sys.Date())
status <- function(type, date = as.Date(NA)) {
  status <- switch(type,
    polishing = "should be readable but needs polishing",
    restructuring = "is undergoing heavy restructuring and may be confusing or incomplete",
    drafting = "is currently a dumping ground for ideas, and we don't recommend reading it",
    complete = "is largely complete and just needs final proof reading",
    stop("Invalid `type`", call. = FALSE)
  )

  if (!is.na(date)) {
    date <- as.Date(date)
    course_info <- paste0(
      "\n**It is part of the course materials intended for ",
      format(date, "%b %d, %Y"),
      "**.\n"
    )
  } else {
    course_info <- ""
  }

  # append_df(data.frame(type = type, date = date))

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
    course_info,
    ":::\n",
    "::::\n"
  ))
}

# # Unclear what's a good way to share date between qmd. This writes a file
# # to ~ for now.
# # append_df(refresh = TRUE)          # delete
# # append_df()                        # show
# # append_df(data.frame(a = "dfsf"))  # append
# append_df <- function(row = NULL,
#                       name = "status",
#                       path = fs::path("~", paste0(name, ".rds")),
#                       refresh = FALSE) {
#   r <- if (file.exists(path)) readr::read_rds(path) else NULL
#   ans <- rbind(r, row)
#   if (is.null(row)) {
#     if (refresh && file.exists(path)) fs::file_delete(path)
#   } else {
#     readr::write_rds(ans, path)
#   }
#   ans
# }
