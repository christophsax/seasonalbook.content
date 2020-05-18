


dta_raw <- read_csv("~/git/snb/seasadj/data/SIC_ts4XCSAX_d_extern.csv", col_types = cols())

count <-
  filter(dta_raw, PaymentCategory == "all") %>%
  transmute(time = SettlementDateSic, value = trx_count / 1e3)


oos_report <- function(x) {
  x_name <- deparse(substitute(x))
  rmarkdown::render(
  "script/oos_eval_report.R",
  output_file = file.path(here::here("out"), paste0(x_name, ".html")),
  params = list(x = x, x_name = x_name))
}


oos_report(count)


oos_report(transact)

