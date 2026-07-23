# ABID

<!-- badges: start -->
<!-- 
[![Static Badge](https://img.shields.io/badge/LiveApp-blue)](https://apps.bam.de/shn01/abid/)
[![CRAN status](https://www.r-pkg.org/badges/version/abid)](https://CRAN.R-project.org/package=abid)
[![R-CMD-check](https://github.com/janlisec/abid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/abid/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/janlisec/abid/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/janlisec/abid/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/janlisec/abid/branch/main/graph/badge.svg)](https://app.codecov.io/gh/janlisec/abid?branch=main)
 -->
[![Static Badge](https://img.shields.io/github/r-package/v/janlisec/abid)](https://img.shields.io/github/r-package/v/janlisec/abid)
[![Static Badge](https://img.shields.io/badge/doi-10.3390/antib11020027-yellow.svg)](https://doi.org/10.3390/antib11020027)
<!-- badges: end -->

This repository contains the R code to run a `Shiny`-App which is described in 
"Tscheuschner et al. (2022) MALDI-TOF-MS-Based Identification of Monoclonal Murine Anti-SARS-CoV-2 Antibodies within One Hour".

It can be installed as an R-package from this GitHub repository and run locally by

``` r
devtools::install_github("janlisec/abid")
abid::abid_app()
```

To confirm the identity of an antibody can be a difficult task. One option is a 
digestion of the antibody followed by a fingerprinting mass spectrometry analysis 
and a comparison of the resulting spectrum against a library of digested 
antibodies. 

The 'ABID' package provides a single function starting a `Shiny`-App for local 
use together with two datasets, a library of spectra of digested antibodies and 
a list of masses typical for certain antibody sub-classes.

The App allows the user to upload similar spectra, one at a time, and compare 
them with the library spectra. It also allows to provide spectra to set up a 
user defined library for comparison.
