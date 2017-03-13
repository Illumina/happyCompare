# library(happyR)
# 
# happy_prefix = "C:/Users/mgonzalez/SublimeProjects/hap.py/example/happy/expected-stratified"
# happyr_stratified = read_happy(happy_prefix)
# happyr_stratified$summary = happyr_stratified$summary %>% 
#   mutate(Group.Id = "A", Replicate.Id = "expected-stratified")
# 
# happy_prefix = "C:/Users/mgonzalez/SublimeProjects/hap.py/example/happy/expected-qfy"
# happyr_qfy = read_happy(happy_prefix)
# happyr_qfy$summary = happyr_qfy$summary %>% 
#   mutate(Group.Id = "B", Replicate.Id = "expected-qfy")
# 
# happy_result_list = c(happyr_stratified, happyr_qfy)
# devtools::use_data(happy_result_list, overwrite = TRUE)

load(system.file("data/happy_result_list.rda", package = "happyCompare"))

# happy_input = system.file("extdata", "happy_demo.summary.csv", package = "happyR")
# happy_prefix = sub(".summary.csv", "", happy_input)
# hapdata = read_happy(happy_prefix)
# hapdata$pr_curve$all = hapdata$pr_curve$all %>% 
#   mutate(Group.Id = "A")
# class(hapdata$pr_curve$all) = c("happy_roc", class(hapdata$pr_curve$all))
# devtools::use_data(hapdata, overwrite = TRUE)

load(system.file("data/hapdata.rda", package = "happyCompare"))