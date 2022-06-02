## Resubmission

This is major release that includes new features in the built-in shiny app, new features in the effect size calculation engine, and new options for specific effect size calculation functions.

## Test environments

* local Windows 10 Education, R 4.2.0
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* r-hub:
  ? Windows Server 2008 R2 SP1, R-release, 32/64 bit
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  ? Fedora Linux, R-devel, clang, gfortran
  ? Debian Linux, R-release, GCC

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1037/met0000019
    From: man/LOR.Rd
          man/LRR.Rd
    Status: 400
    Message: Bad Request
  URL: https://doi.org/10.1177/001440290907500201
    From: man/IRD.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/00224669070400040101
    From: man/PAND.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0145445504272974
    From: man/PEM.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0145445511399147
    From: man/PAND.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0145445516676750
    From: man/Tau_BC.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/074193258700800206
    From: man/PND.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.3102/1076998620934125
    From: man/LRM.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1002/pits.20516
    From: inst/doc/Using-SingleCaseES.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1002/sim.2256
    From: inst/doc/Effect-size-definitions.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1037/1082-989X.6.2.135
    From: inst/doc/Effect-size-definitions.html
    Status: 400
    Message: Bad Request
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

We checked 1 reverse dependency, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
