## Resubmission

This is a maintenance release, which includes small modifications to some methods to use truncated sample variances, ensuring that the functions return standard errors strictly greater than zero. It also corrects a problem in the unit tests that led to errors in CRAN check on M1mac.

## Test environments

* local Windows 7 Enterprise, R 4.0.2
* ubuntu 16.04.6 LTS (on travis-ci), R devel, release, oldrelease
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
URL: http://doi.org/10.1080/01621459.1990.10474942 (moved to https://doi.org/10.1080/01621459.1990.10474942)
    From: man/NAP.Rd
    Status: 200
    Message: OK
  URL: https://doi.org/10.2307/1165329
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
  URL: https://www.jstor.org/stable/25049448
    From: man/NAP.Rd
    Status: 403
    Message: Forbidden
    
  The flagged URLs are correct.

## Reverse dependencies

There are no downstream dependencies for this package.
