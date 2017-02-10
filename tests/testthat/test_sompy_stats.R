context("sompy stats")

test_that("demo data are loaded in the expected format", {
    demo_sompy_stats = happyCompare::demo_haplocompare_somatic
    dst = demo_sompy_stats$sompy_stats
    
    expect_true(is_sompy_stats(dst))
    
    expect_equal(dim(dst[[1]])[1], 12)
    expect_equal(dim(dst[[1]])[2], 21)
})

test_that("tidy.sompy_stats works", {
    demo_sompy_stats = happyCompare::demo_haplocompare_somatic
    dst = demo_sompy_stats$sompy_stats
    tdst = tidy(dst)
    expect_is(tdst, "data.table")
    expect_equal(dim(tdst)[1], 72)
    expect_equal(dim(tdst)[2], 21)
})

test_that("plot.sompy_stats works", {
    demo_sompy_stats = happyCompare::demo_haplocompare_somatic
    hs = demo_sompy_stats$sompy_stats
    p = plot(hs, type = 'SNVs')
    expect_is(p, "gtable")
})

test_that("plot_af.sompy_stats works", {
    demo_sompy_stats = happyCompare::demo_haplocompare_somatic
    hs = demo_sompy_stats$sompy_stats
    p = plot_af(hs, type = 'SNVs')
    expect_is(p, "gtable")
})

test_that("summary.sompy_stats works", {
    demo_sompy_stats = happyCompare::demo_haplocompare_somatic
    hs = demo_sompy_stats$sompy_stats
    s = summary(hs, type = 'SNVs')
    expect_is(s, "knitr_kable")
})
