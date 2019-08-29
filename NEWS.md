# SingleCaseES 0.4.3

* Updated internal functions for compatibility with tidyr 1.0.0.
* Tweaks for SCD_effect_sizes() shiny app.
    * Corrected bug in the single-series calculator that ignored direction of improvement for parametric effect sizes.
    * Fixed bug in the batch calculator phase-selection drop-down menus.
    * Corrected bug in the batch calculator so that confidence intervals other than 95% could be calculated.
    * Added a black-box warning any time PND is calculated.
    * Added option to convert LRRi and LRRd to percentage change.

# SingleCaseES 0.4.2

* Added check for purrrlyr package when loading gem_scd calculator.
* Corrected bug in degrees of freedom for within-case standardized mean difference with baseline SD.
* Tweaks for SCD_effect_sizes() shiny app.
    * Improved UI for batch calculation mode.
    * Added the option to select a treatment phase for batch calculation mode.
    * Allowed for choice between baseline SD and pooled SD for the within-case standardized mean difference. Previously, the option was available but not actually used for calculation.

# SingleCaseES 0.4.1

* Fixed bug in SCD_effect_sizes() shiny app that caused an error in batch calculator when LRRi, LRRd, or LOR were not selected.
* calc_ES() now returns a zero-length data frame if condition and outcome arguments are both length-zero vectors. 
* calc_ES() errors if condition and outcome arguments are not the same length.

# SingleCaseES 0.4.0

* Added calc_ES() function for calculating multiple effect size estimates on a single data series.
* Added batch_calc_ES() function for calculating effect size estimates for multiple data series.
* Added gem_scd() function for estimating the gradual effects model (Swan & Pustejovsky, 2018).
* Added shiny app for calculating basic within-case effect sizes.
* Added shiny app for estimating effect sizes based on gradual effects model.
* Revised documentation.
* Added unit tests.
* Added vignettes.

# SingleCaseES 0.3

* Fixed definitions of PAND and IRD to handle cases of complete overlap (i.e., where all of the data from a phase must be removed to achieve non-overlap).

# SingleCaseES 0.2

* Initial release.
