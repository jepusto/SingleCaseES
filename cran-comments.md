## Resubmission

This is minor release that includes small bug-fixes in package functions and in the built-in shiny app.

## Test environments

* local Windows 10 Education, R 4.2.2
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder, R release

## R CMD check results

There were no ERRORs or WARNINGs. 

There were 3 NOTEs:

Possibly mis-spelled words in DESCRIPTION:
  Pustejovsky (15:15)
  indices (12:58, 18:3)
  
  The flagged words are spelled correctly.
  
Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1002/pits.20516
    From: inst/doc/Using-SingleCaseES.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1002/sim.2256
    From: inst/doc/Effect-size-definitions.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1044/2022_JSLHR-22-00217
    From: inst/doc/Effect-size-definitions.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1080/00273171.2018.1466681
    From: inst/doc/Effect-size-definitions.html
          inst/doc/Using-SingleCaseES.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1080/17489539.2020.1732024
    From: inst/doc/Effect-size-definitions.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1148/radiology.143.1.7063747
    From: inst/doc/Effect-size-definitions.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1165329
    From: inst/doc/Effect-size-definitions.html
    Status: 403
    Message: Forbidden
  URL: https://www.jstor.org/stable/25049448
    From: man/NAP.Rd
    Status: 403
    Message: Forbidden
    
  The flagged URLs are correct.

Found the following (possibly) invalid DOIs:
  DOI: 10.1080/00273171.2018.1466681
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
    
  The flagged DOI is correct.
  
## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
