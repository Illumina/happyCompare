context("haplocompare")

test_that("demo data are loaded in the expected format", {
    demo_haplocompare = happyCompare::demo_haplocompare_germline
    
    expect_true(is_haplocompare(demo_haplocompare))
    expect_equal(sum(!sapply(demo_haplocompare, is.null)), 4)
})

test_that("merge_nested.haplocompare works", {
    demo_haplocompare = happyCompare::demo_haplocompare_germline
    
    merged_dt = merge_nested(obj = demo_haplocompare, x = 'happy_summary', y = 'build_metrics', 
                             select = c('Type', 'Filter', 'METRIC.Recall', 'autosome_mean_coverage'))
    
    expect_is(merged_dt, "data.table")
    expect_equal(dim(merged_dt)[1], 16)
    expect_equal(dim(merged_dt)[2], 8)
})
