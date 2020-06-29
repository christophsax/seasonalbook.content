
oos_report <- function(x, x_name = deparse(substitute(x))) {

  rmarkdown::render(
  "script/oos_eval_report.R",
  output_file = file.path(here::here("out"), paste0(x_name, ".html")),
  params = list(x = x, x_name = x_name))
}


### SNB

dta_raw <- read_csv("~/git/snb/seasadj/data/SIC_ts4XCSAX_d_extern.csv", col_types = cols())


count <-
  filter(dta_raw, PaymentCategory == "all") %>%
  transmute(time = SettlementDateSic, value = trx_count / 1e3)

count_inter <-
  filter(dta_raw, PaymentCategory == "Interbank") %>%
  transmute(time = SettlementDateSic, value = trx_count / 1e3)

count_retail <-
  filter(dta_raw, PaymentCategory == "Retail") %>%
  transmute(time = SettlementDateSic, value = trx_count / 1e3)


transact <-
  filter(dta_raw, PaymentCategory == "all") %>%
  transmute(time = SettlementDateSic, value = trx_CHF_sum / 1e6)

transact_inter <-
  filter(dta_raw, PaymentCategory == "Interbank") %>%
  transmute(time = SettlementDateSic, value = trx_CHF_sum / 1e6)

transact_retail <-
  filter(dta_raw, PaymentCategory == "Retail") %>%
  transmute(time = SettlementDateSic, value = trx_CHF_sum / 1e6)

save(count, count_inter, count_retail, transact_inter, transact_retail, file = "data/snb-data.RData")

oos_report(count)
oos_report(count_inter)
oos_report(count_retail)
oos_report(transact)
oos_report(transact_inter)
oos_report(transact_retail)


### Trendecon


trendecon <- read_csv("data/trendecon.csv")

x_names <-
  c("Zara", "pizza bestellen", "Heim+Hobby", "christ", "Fahrplan",
"Kino", "städtetrip", "Kurzarbeit")


ser <- function(x_name) {
  ts_pick(trendecon, x_name) %>%
    select(-keyword)
}

for (x_name in x_names) {
  message(x_name, " ----------------------------------------------")
  oos_report(ser(x_name), x_name)
}



oos_report(count_retail, "test")

zz <- ser("Zara")

m <- seas_loess9(ser("Zara"))

plot_components(m)


seas_dummy(zz)



system.time(seas_loess9(transact))
system.time(seas_dsa(transact))
system.time(seas_prophet(transact))



seas_loess9:  0.639
seas_dummy:   2.319
prophet:     10.181
dsa:        145.553


seas_duu(transact))
Error in seas_prophe(transact) : could not find function "seas_prophe"
Timing stopped at: 0 0 0
>
> system.time(seas_dummy(transact))
   user  system elapsed
  9.845   0.333


# seas_loess7(ser(x_name))
# seas_dummy(ser(x_name))

# x <- ser(x_name)

# seas_dummy(transact)

# count <-
#   filter(dta_raw, PaymentCategory == "all") %>%
#   transmute(time = SettlementDateSic, value = trx_count / 1e3)




# # count2 <- count
# oos_report(count, "count_test2")
# oos_report(transact)

