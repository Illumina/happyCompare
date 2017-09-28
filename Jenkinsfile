#!/usr/bin/env groovy

node('uk_centos6_cluster') {

    env.PATH = "/illumina/thirdparty/R/R-3.2.3/bin:${env.PATH}"
    env._R_CHECK_FORCE_SUGGESTS_ = false
    
    // custom R packages (e.g. happyR) installed here
    env.R_LIBS_USER = "/illumina/thirdparty/bmoore1/rlibs-3.2.3"

    // centos 6.5 compiled pandoc binary
    env.RSTUDIO_PANDOC = '/illumina/development/curium/bin'
    
    stage('Checkout') {
        checkout scm
    }

    stage('Build') {
        sh "R CMD build --no-build-vignettes ."
    }

    stage('Test') {
        // Full test + check
        // sh "R CMD check --no-examples happyCompare_*.tar.gz"
        
        // Test only
        sh "R -e 'devtools::test()'"
    }
    
    stage('Test coverage') {
        sh "Rscript -e 'covr::report(covr::package_coverage(), file=\"/illumina/development/www/python/codecov/static/happyCompare.html\", browse = F)'"
    }    

    stage('Cleanup') {
        deleteDir()
    }

}
