library("happyCompare")
library("ggplot2")
library("dplyr")

# inspect the pre-loaded demo_happy_extended dataset
class(demo_happy_extended)
print(demo_happy_extended)

# inspect the happy extended results
he = demo_happy_extended$happy_extended
class(he)
head(he, n = 2)

# apply happy_extended methods
head(tidy(he))
