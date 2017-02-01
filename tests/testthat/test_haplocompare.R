context("haplocompare")

test_that("demo data are loaded in the expected format", {
    setwd(path.package('happyCompare'))
    
    config_path = "data-raw/config.haplocompare.csv"
    demo_haplocompare = load_data(config_path = config_path)
    
    expect_true(is_haplocompare(demo_haplocompare))
    expect_equal(sum(!sapply(demo_haplocompare, is.null)), 3)
})

test_that("merge_nested.haplocompare works", {
    merged_dt = merge_nested(obj = demo_haplocompare, x = 'happy_summary', y = 'build_metrics', 
                             select = c('Type', 'Filter', 'METRIC.Recall', 'autosome_mean_coverage'))
    
    expect_is(merged_dt, "data.table")
    expect_equal(dim(merged_dt)[1], 16)
    expect_equal(dim(merged_dt)[2], 8)
})
