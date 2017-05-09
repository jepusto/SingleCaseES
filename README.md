SingleCaseES: Effect sizes for single-case designs
==================================================

This package provides R functions for calculating basic effect size indices for single-case designs, including several non-overlap measures and parametric effect size measures. Standard errors and confidence intervals for the effect sizes are provided for a subset of indices with known sampling distributions. However, it is important to note that all of the standard errors and confidence intervals are based on the assumption that the outcome measurements are mutually independent.

The available **non-overlap indices** are:

-   Percentage of non-overlapping data (PND)
-   Percentage of all non-overlapping data (PAND)
-   Robust improvement rate difference (IRD)
-   Percentage exceeding the median (PEM)
-   Non-overlap of all pairs (NAP)
-   Tau non-overlap (Tau)
-   Tau-U (including baseline trend adjustment)

The available **parametric effect sizes** are:

-   Within-case standardized mean difference
-   Log response ratio

The package also includes a graphical user interface (designed using [Shiny](https://shiny.rstudio.com/)) for interactive use. The GUI is also available [as a web app](jepusto.shinyapps.io/SCD-effect-sizes), hosted through [shinyapps.io](https://www.shinyapps.io/). However, the web app should only be used for demonstration purposes. For research purposes, please install the R package and run the GUI through Rstudio.

Installation
============

The package is currently only available on Github. To install it, you will first need to [install R](http://cran.r-project.org/) and [RStudio](http://www.rstudio.com/products/rstudio/download/). Windows users will also need to [install Rtools](http://cran.r-project.org/bin/windows/Rtools/). All of these programs are freely availble.

Once you have these programs installed, run the following commands at the RStudio console prompt:

``` r
install.packages("devtools")
install.packages("sourcetools")
install.packages("shiny")
install.packages("markdown")
install.packages("ggplot2")
devtools::install_github("jepusto/SingleCaseES")
```

Basic usage
===========

After installing the package, you are ready to begin using it. Every time you open a fresh R/RStudio session, you will need to start by loading the package functionality as follows:

``` r
library(SingleCaseES)
```

The functions for calculating the non-overlap indices and parametric effect size estimates all have the same basic structure. Each function takes two inputs: a vector of outcome values for the baseline phase and a vector of outcome values for the treatment phase. For example, the following code creates an object `A` containing outcomes from a baseline phase an an object `B` containing outcomes from a treatment phase:

``` r
A <- c(20, 20, 26, 25, 22, 23)
B <- c(28, 25, 24, 27, 30, 30, 29)
```

NAP
---

After inputing the data, non-overlap of all pairs can be calculated as follows:

``` r
NAP(A, B, improvement = "increase")
```

    ## $Est
    ## [1] 0.9166667
    ## 
    ## $SE
    ## [1] 0.07739185
    ## 
    ## $CI
    ##     lower     upper 
    ## 0.5973406 0.9860176

Because formulas are available for NAP, the function reports the standard error and confidence interval in addition to the point estimate of NAP. Note that these formulas assume that the outcome measurements are mutually independent. In the presence of positive auto-correlation, they may under-estimate the true sampling variability of NAP.

The `NAP` function assumes that increases in the value of the outcome represent therapeutic improvements. For outcomes where decrease is therapeutically desirable, set the optional argument `improvement` to `"decrease"`:

``` r
NAP(A, B, improvement = "decrease")
```

    ## $Est
    ## [1] 0.08333333
    ## 
    ## $SE
    ## [1] 0.07739185
    ## 
    ## $CI
    ##      lower      upper 
    ## 0.01398242 0.40265939

Type `?NAP` at the console prompt for further information about the available options for NAP.

Other non-overlap indices
-------------------------

Other effect size calculation functions work very similarly. For example, the robust improvement rate difference can be calculated as follows:

``` r
IRD(A, B, improvement = "increase")
```

    ## [1] 0.6904762

Note that only a point estimate is returned because (to the author's knowledge) there are not available methods for estimating the standard error or confidence interval of IRD.

Parametric effect size indices
------------------------------

The function for calculating the within-case standardized mean difference has similar arguments to the non-overlap indices:

``` r
SMD(A, B)
```

    ## $Est
    ## [1] 1.703734
    ## 
    ## $SE
    ## [1] 0.6370139
    ## 
    ## $CI
    ## [1] 0.4552097 2.9522583

By default, the estimate is calculated using the standard deviation of the baseline phase data only. To use the standard deviation pooled across both phases, set the `std_dev` option to `"pool"`:

``` r
SMD(A, B, std_dev = "pool")
```

    ## $Est
    ## [1] 1.876247
    ## 
    ## $SE
    ## [1] 0.6374216
    ## 
    ## $CI
    ## [1] 0.6269241 3.1255707

By default, the estimate is calculated using the small-sample correction proposed by Hedges (1981) and often referred to as "Hedges' *g*." The standard error and approximate confidence interval for the standardized mean difference estimate are based on the assumption that the outcome measurements are mutually independent.

The log response ratio can be calculated as follows:

``` r
LRR(A, B)
```

    ## $Est
    ## [1] 0.1953962
    ## 
    ## $SE
    ## [1] 0.05557723
    ## 
    ## $CI
    ## [1] 0.08646679 0.30432554

By default, the estimate is calculated using a small-sample correction proposed by Pustejovsky (2015). See `?LRR` for further details. The standard error and approximate confidence interval for the log response ratio estimate are based on the assumption that the outcome measurements are mutually independent.

Graphical user interface
========================

To use the graphical user interface, you must first ensure that the `SingleCaseES` package is installed (following the directions above). To start the simulator, type the following commands at the RStudio console prompt:

``` r
library(SingleCaseES)
SCD_effect_sizes()
```

The simulator should then open in your default web browser.

To exit the calculator, close the window in which it appears and click the red "Stop" icon in the upper right-hand corner of the RStudio console window.
