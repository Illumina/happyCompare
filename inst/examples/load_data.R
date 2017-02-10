## demo datasets

library("happyCompare")
library("dplyr")

config_path = "inst/examples/config.haplocompare_germline.csv"
demo_haplocompare_germline = load_data(config_path = config_path)
devtools::use_data(demo_haplocompare_germline, overwrite = TRUE)

he = demo_haplocompare_germline$happy_extended
demo_happy_extended_ci = add_credible_intervals(he, metric = 'METRIC.Recall', samplesize = 1e5)
devtools::use_data(demo_happy_extended_ci, overwrite = TRUE)

config_path = "inst/examples/config.haplocompare_somatic.csv"
demo_haplocompare_somatic = load_data(config_path = config_path)
devtools::use_data(demo_haplocompare_somatic, overwrite = TRUE)

## data can be accessed using one of the following
##
## option 1:
# library("happyCompare")
# demo_happy_summary
##
## option 2:
# happyCompare::demo_happy_summary
##
## option 3:
# data("demo_happy_summary", package="happyCompare")
# demo_happy_summary