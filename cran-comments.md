## Resubmission

This is major release that includes new effect size calculation functions, new post-processing features, and updates to the built-in shiny app.

## Test environments

* local Windows 10 Education, R 4.0.3
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* r-hub:
  * Windows Server 2008 R2 SP1, R-release, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran
  * Debian Linux, R-release, GCC

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
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  URL: https://jepusto.shinyapps.io/gem-scd
    From: inst/doc/Using-SingleCaseES.html
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  URL: https://doi.org/10.2307/1165329
    From: inst/doc/Effect-size-definitions.html
    Status: 403
    Message: Forbidden
  URL: https://www.jstor.org/stable/25049448
    From: man/NAP.Rd
    Status: 403
    Message: Forbidden
    
  The flagged URLs are correct.

## Reverse dependencies

There are no downstream dependencies for this package.
