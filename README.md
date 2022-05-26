<!-- badges: start -->

[![R-CMD-check](https://github.com/jepusto/SingleCaseES/workflows/R-CMD-check/badge.svg)](https://github.com/jepusto/SingleCaseES/actions)
[![Codecov
Status](https://codecov.io/gh/jepusto/SingleCaseES/branch/master/graph/badge.svg)](https://codecov.io/gh/jepusto/SingleCaseES?branch=master)
[![](http://www.r-pkg.org/badges/version/SingleCaseES)](https://CRAN.R-project.org/package=SingleCaseES)
[![](http://cranlogs.r-pkg.org/badges/grand-total/SingleCaseES)](https://CRAN.R-project.org/package=SingleCaseES)
[![](http://cranlogs.r-pkg.org/badges/last-month/SingleCaseES)](https://CRAN.R-project.org/package=SingleCaseES)
<!-- badges: end -->

# SingleCaseES: A calculator for single-case effect size indices

This package provides R functions for calculating basic effect size
indices for single-case designs, including several non-overlap measures
and parametric effect size measures, and for estimating the gradual
effects model developed by [Swan and Pustejovsky
(2018)](https://doi.org/10.1080/00273171.2018.1466681). Standard errors
and confidence intervals (based on the assumption that the outcome
measurements are mutually independent) are provided for the subset of
effect sizes indices with known sampling distributions.

The available **non-overlap indices** are:

-   Percentage of non-overlapping data (PND)
-   Percentage of all non-overlapping data (PAND)
-   Robust improvement rate difference (IRD)
-   Percentage exceeding the median (PEM)
-   Non-overlap of all pairs (NAP)
-   Tau non-overlap (Tau)
-   Baseline-corrected Tau (Tau-BC)
-   Tau-U (including baseline trend adjustment)

The available **parametric effect sizes** are:

-   Within-case standardized mean difference
-   Log response ratio (decreasing and increasing)
-   Log odds ratio
-   Log ratio of medians
-   The gradual effects model, which can be used to estimate log
    response ratios or log odds ratios in the presence of time trends
    during treatment and return-to-baseline phases.

The package also includes two graphical user interfaces (designed using
[Shiny](https://shiny.rstudio.com/)) for interactive use, both of which
are also available as web apps hosted through
[shinyapps.io](https://www.shinyapps.io/):

-   `SCD_effect_sizes()` opens an interactive calculator for the basic
    non-overlap indices and parametric effect sizes. It is also
    available at <https://jepusto.shinyapps.io/SCD-effect-sizes>
-   `shine_gem_scd()` opens an interactive calculator for the gradual
    effects model. It is also available at
    <https://jepusto.shinyapps.io/gem-scd>

***Please note that the web apps should only be used for demonstration
purposes***. For research purposes, please install the R package and run
the GUI through RStudio.

# Installation

The package is available on the Comprehensive R Archive Network. To
install it, you will first need to [install
R](http://cran.r-project.org/) and
[RStudio](http://www.rstudio.com/products/rstudio/download/). Both of
these programs are freely available. Once you have these programs
installed, run the following commands at the RStudio console prompt:

``` r
install.packages("SingleCaseES")
```

To install the latest development version directly from Github, type:

``` r
install.packages("devtools")
install.packages("sourcetools")
install.packages("shiny")
install.packages("markdown")
install.packages("ggplot2")
install.packages("purrrlyr")
devtools::install_github("jepusto/SingleCaseES", build_vignettes = TRUE, force = TRUE)
```

# Getting started

The package includes two vignettes that demonstrate the syntax of the
main functions and provide precise definitions and details about how
each of the effect sizes are calculated. To view the vignettes, type the
following:

``` r
browseVignettes("SingleCaseES")
```

A list of vignettes should then appear in your browser. Click on the
hyperlinks to view them. Alternately, type the following line of code to
open the introductory vignette:

``` r
vignette("Using-SingleCaseES")
```

# Graphical user interface

To use the graphical user interface for basic effect sizes, you must
first ensure that the `SingleCaseES` package is installed (following the
directions above). To start the calculator, type the following commands
at the RStudio console prompt:

``` r
library(SingleCaseES)
SCD_effect_sizes()
```

The calculator should then open in your default web browser. To exit the
calculator, close the window in which it appears.

To use the graphical user interface for the gradual effects model, type
the following commands at the RStudio console prompt:

``` r
library(SingleCaseES)
shine_gem_scd()
```

The calculator should then open in your default web browser.

# Acknowledgement

| ![](https://raw.githubusercontent.com/jepusto/SingleCaseES/master/images/IES_InstituteOfEducationSciences_RGB.svg) | The development of this R package was supported in part by the Institute of Education Sciences, U.S. Department of Education, through [Grant R324U190002](https://ies.ed.gov/funding/grantsearch/details.asp?ID=3358) to the University of Oregon. The contents of the package do not necessarily represent the views of the Institute or the U.S. Department of Education. |
|:-----------------------------------|:-----------------------------------|

# Citations

Please cite this R package as follows:

> Pustejovsky, J. E., Chen, M., & Swan, D. M. (2021). SingleCaseES: A
> calculator for single-case effect size indices. R package version
> 0.5.0. Retrieved from <https://github.com/jepusto/SingleCaseES>

Please cite the web applications as follows:

> Pustejovsky, J. E., Chen, M., & Swan, D. M. (2021). Single-case effect
> size calculator (Version 0.5.0) \[Web application\]. Retrieved from
> <https://jepusto.shinyapps.io/SCD-effect-sizes>

> Swan, D. M. & Pustejovsky, J. E. (2017). gem_scd: A web-based
> calculator for the Gradual Effects Model (Version 0.1.0) \[Web
> application\]. Retrieved from: <https://jepusto.shinyapps.io/gem-scd>
