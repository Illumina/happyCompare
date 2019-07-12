
happyCompare
============

**This repository has been merged with [happyR](https://github.com/Illumina/happyR), and will no longer be updated.**

[![Build Status](https://travis-ci.org/Illumina/happyCompare.svg?branch=master)](https://travis-ci.org/Illumina/happyCompare) [![Coverage Status](https://codecov.io/github/Illumina/happyCompare/coverage.svg?branch=master)](https://codecov.io/github/Illumina/happyCompare)

happyCompary offers a set of functions to facilitate downstream analysis of variant calling performance outputs from [hap.py](https://github.com/Illumina/hap.py). It builds on top of [happyR](https://github.com/Illumina/happyR) to support annotation of hap.py results (e.g. grouping) through metadata samplesheets, and provides methods for quick retrieval, statistical analysis and easy reporting of performance metrics.

Install
-------

``` r
devtools::install_github("Illumina/happyCompare")
```

Usage
-----

Read the [Quick start guide](https://illumina.github.io/happyCompare/articles/quick_start.html) for summarised instructions on how to load data from `happyCompare` samplesheets, access metrics and visualise results.

System requirements
-------------------

Development and testing for `happyCompare` have been done using R 3.3.3 on a Centos 6.9 machine, with 2 processor cores and 8GB of RAM. Alternative systems are also supported as documented in the official [R installation guide](https://cran.r-project.org/doc/manuals/r-release/R-admin.html).
