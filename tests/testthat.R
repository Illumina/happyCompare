library(testthat)
library(happyCompare)

test_check("happyCompare")
load(system.file("demo_data/happyCompare_list.RData", package = "happyCompare"))
load(system.file("demo_data/happy_extended.RData", package = "happyCompare"))
