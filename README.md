README
================

-   [happyCompare](#happycompare)
    -   [Examples](#examples)
        -   [Static reports](#static-reports)

happyCompare
============

Reporting toolbox for happy output

Examples
--------

### Static reports

**Example germline report:** [germline.html](inst/examples/germline.html)

``` bash
HAPPYR_HOME="C:/Users/mgonzalez/SublimeProjects/happyR"
HAPPYCOMPARE_HOME="C:/Users/mgonzalez/SublimeProjects/happyCompare_master"
Rscript --vanilla $HAPPYCOMPARE_HOME/exec/happyCompare.R \
        --input_template $HAPPYCOMPARE_HOME/inst/rmd/germline.Rmd \
        --output_dir $HAPPYCOMPARE_HOME/inst/examples \
        --happy_prefix $HAPPYR_HOME/inst/extdata/happy_demo
```

    ## [DONE] Output: C:/Users/mgonzalez/SublimeProjects/happyCompare_master/inst/examples
