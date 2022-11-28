library(haven)

data_raw = haven::read_sav("ESS9e03_1.sav")
summary(data_raw$agea)
data_raw[data_raw$cntry == "DE",]
table(data_raw$cntry)
