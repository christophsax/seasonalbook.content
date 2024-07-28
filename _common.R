set.seed(1014)

suppressMessages(library(tibble))
suppressMessages(library(dplyr))
suppressMessages(library(seasonal))
suppressMessages(library(seasonalbook))

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  fig.width = 9,
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
status <- function(type, date = as.Date(NA), section = "") {
  return(invisible(NULL))
  status <- switch(type,
    polishing = "should be readable but needs polishing",
    restructuring = "is undergoing heavy restructuring and may be confusing or incomplete",
    drafting = "is currently a dumping ground for ideas, and we don't recommend reading it",
    course_complete = "is complete enough as an education tool and can be used in a course. It needs additional polishing",
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

  append_df(data.frame(section = section, type = type, date = date, timestamp = Sys.time()))

  class <- switch(type,
    polishing = "note",
    restructuring = "important",
    drafting = "important",
    course_complete = "note",
    complete = "note"
  )

  cat(paste0(
    "\n",
    ":::: status\n",
    "::: callout-", class, " \n",
    "You are reading a draft of *Seasonal Adjustment in R*. ",
    "This chapter ", status, ".\n",
    course_info,
    ":::\n",
    "::::\n"
  ))
}


todo <- function(x, assignee = "both") {
  ans <- glue::glue(
    "
::: {{.callout-note}}

### Todo

{x}

**Assignee: {assignee}**

:::

    "
  )
  cat(ans)
}


# Unclear what's a good way to share date between qmd. This writes a file
# to ~ for now.
# append_df(refresh = TRUE)          # delete
# append_df()                        # show
# append_df(data.frame(a = "dfsf"))  # append
append_df <- function(row = NULL,
                      name = "status",
                      path = fs::path("~", paste0(name, ".rds")),
                      refresh = FALSE) {
  r <- if (file.exists(path)) readr::read_rds(path) else NULL
  ans <- rbind(r, row)
  if (is.null(row)) {
    if (refresh && file.exists(path)) fs::file_delete(path)
  } else {
    readr::write_rds(ans, path)
  }
  ans
}



# Spec tables ------------------------------------------------------------------

specs_tbl_raw <- function() {
  ll <- jsonlite::read_json(file.path("specs", "specs.json"), simplifyVector = TRUE)
  as_tibble(ll)
}

backtick <- function(x) {
  if (is.null(x)) return(x)
  z <- paste0("`", x, "`")
  # no empty ``
  z[nchar(x) == 0] <- x
  z
}

collapse <- function(x) {
  paste(x, collapse = ", ")
}

bold <- function(x) {
  if (length(x) == 0) return(0)
  paste0("**", x, "**")
}

#' @export
specs_tbl <- function(topic = NULL, caption = NULL) {

  ans <- specs_tbl_raw()

  if (!is.null(topic)) {
    ans <-
      ans |>
      dplyr::filter(topic %in% !! topic)
  }

  format_value <- function(value, default) {
    if (is.null(value)) return("")
    value <- as.character(value)
    if (is.null(default)) {
      default == ""
    }

    value_bt <- backtick(value)
    value_bt[value == default] <- bold(value_bt[value == default])
    collapse(value_bt)
  }

  purrr::map2_chr(ans$value, ans$default, format_value)

  ans <-
    ans |>
    mutate(
      arg = backtick(arg),
      descr = descr,
      value = purrr::map2_chr(value, default, format_value)
    ) |>
    dplyr::transmute(
      Arguments = arg,
      Description = descr,
      `Example values` = value
    )

  ans <- df_to_md(ans)

  # Add the caption if provided
  if (!is.null(caption)) {
    ans <- paste0("Table: ", caption, "\n\n", ans)
  }

  cat(ans)

}


df_to_md <- function(data) {
  headers <- colnames(data)
  header_row <- paste0("| ", paste(headers, collapse = " | "), " |")
  separator_row <- paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|")
  data_rows <- apply(data, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  })
  paste(c(header_row, separator_row, data_rows), collapse = "\n")
}

