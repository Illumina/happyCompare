library("happyCompare")
library("ggplot2")
library("dplyr")

# inspect the pre-loaded demo_happy_summary dataset
class(demo_happy_summary)
print(demo_happy_summary)

# inspect the happy summary results
hs = demo_happy_summary$happy_summary
class(hs)
head(hs)

# apply happy_summary methods
head(tidy(hs))

pdf(file = 'examples/demo_happy_summary.pdf', width = 12, height = 5)
plot(hs, type = 'SNP')
dev.off()

summary(hs, type = 'SNP')