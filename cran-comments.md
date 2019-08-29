## Resubmission

This is a maintenance release, which includes updates to internal functions for compatibility with tidyr 1.0.0 and user interface improvements for an embedded shiny app.

## Test environments

* local Windows 7 Enterprise, R 3.6.0
* ubuntu 14.04.5 LTS (on travis-ci), R 3.6.0, devel
* win-builder (devel, release, oldrelease)
* r-hub:
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran


## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1165329 (moved to http://www.jstor.org/stable/1165329)
    From: inst/doc/Effect-size-definitions.html
    Status: 403
    Message: Forbidden
  URL: https://jepusto.shinyapps.io/SCD-effect-sizes
    From: inst/doc/Using-SingleCaseES.html
    Status: Error
    Message: libcurl error code 35:
      	Unknown SSL protocol error in connection to jepusto.shinyapps.io:443
  URL: https://jepusto.shinyapps.io/gem-scd
    From: inst/doc/Using-SingleCaseES.html
    Status: Error
    Message: libcurl error code 35:
      	Unknown SSL protocol error in connection to jepusto.shinyapps.io:443
  URL: https://www.jstor.org/stable/25049448
    From: man/NAP.Rd
    Status: 403
    Message: Forbidden

  The flagged URLs are correct.

## Reverse dependencies

There are no downstream dependencies for this package.
