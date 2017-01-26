## demo datasets

library("happyCompare")
library("dplyr")
setwd(path.package('happyCompare'))

config_path = "data-raw/config.happy_summary.csv"
demo_happy_summary = load_data(config_path = config_path)
devtools::use_data(demo_happy_summary, overwrite = TRUE)

config_path = "data-raw/config.happy_extended.csv"
demo_happy_extended = load_data(config_path = config_path)
devtools::use_data(demo_happy_extended, overwrite = TRUE)

he = demo_happy_extended$happy_extended
he_ci = add_credible_intervals(he, metric = 'METRIC.Recall', samplesize = 1e5)
devtools::use_data(he_ci, overwrite = TRUE)

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