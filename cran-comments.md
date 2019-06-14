## Resubmission

This is a resubmission and maintenance release, which includes miscellaneous bug fixes and user interface improvements for an embedded shiny app.

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

* Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.2307/1165329 (moved to http://www.jstor.org/stable/1165329)
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
