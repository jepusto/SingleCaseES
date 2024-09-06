## Resubmission

This is minor release that includes small bug-fixes in package functions and in the built-in shiny app.

## Test environments

* local Windows 11 x64, R 4.4.0
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder, R release

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

Found the following (possibly) invalid URLs:
  URL: https://www.jstor.org/stable/25049448
    From: man/NAP.Rd
    Status: 403
    Message: Forbidden
    
  The flagged URL is correct.

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
