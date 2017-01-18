library(dplyr)

config_path = "data-raw/config.demo_happy_summary.csv"
demo_happy_summary = load_data(config_path = config_path)
devtools::use_data(demo_happy_summary)

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