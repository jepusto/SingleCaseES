# SingleCaseES 0.7.1.9999

* The package and SCD_effect_sizes app now accept uppercase outcome measurement scales for LRRi, LRRd, or LOR effect sizes.
* The package and SCD_effect_sizes app now accept uppercase labels for the direction of improvement variable.
* Updates to the multiple-series calculator in the SCD_effect_sizes app:
    * The app now shows a warning message if the improvement direction variable includes a category that is not acceptable.
    * The app now shows a warning message if a measurement scale is not acceptable for LRRi, LRRd, or LOR effect sizes.


# SingleCaseES 0.7.1

* Fixed a bug in the SCD_effect_sizes app (multiple-series calculator) which led to an error when calculating `PoGO` from an uploaded .xlsx file.
* Added several example datasets to the package.

# SingleCaseES 0.7.0

* Fixed a bug in the SCD_effect_sizes app (multiple-series calculator), which led to incorrect R syntax when `phase_pair_calculated` is included as a grouping variable.
* Added function for calculating Percent of Goal Obtained effect size estimate.
* Updates to SCD_effect_sizes() shiny app.
    * Added Percent of Goal Obtained effect size measure to single-series calculator and multiple-series calculator.
* Changes to internal functions because of syntax changes in dplyr 1.1.0.

# SingleCaseES 0.6.1

* Revised in-app documentation for the SCD_effect_sizes web app. Added links to video demonstrations.
* Changes to the SCD_effect_sizes shiny app
    * The multiple series calculator gains an option for changing the number of digits displayed in the output.
    * Fixed a bug in the SCD_effect_sizes app (multiple-series calculator) plotting facilities, which caused an error if the phase variable was numeric.
    * Fixed a bug in the multiple-series calculator so that `bintervals`, `bobslength`, and `blrrfloor` are used for calculating LOR and LRR effect sizes.
    * Added an option to calculate phase pair numbers in the multiple-series calculator for uploaded data sets.
* Added informative error messages to the truncation constant calculations.


# SingleCaseES 0.6.0

* Changes to batch_calc_ES()
    * batch_calc_ES() now provides several additional options for weights to use when aggregating effect sizes, including weighting by nA (the number of baseline phase observations), nB (the number of treatment phase observations), nA * nB, or 1 / nA + 1 / nB.
    * For aggregating effect sizes, batch_calc_ES() now defaults to equal-weighting (rather than inverse-variance weighting)
    * For aggregating effect sizes, batch_calc_ES() now provides more informative error messages for effect sizes that do not have standard errors (and therefore cannot be aggregated with inverse-variance weights).
    * Corrected a cosmetic bug in a warning message of batch_calc_ES() when scale argument is specified as a variable and ES includes "LOR".
* Changes to functions for specific effect sizes
    * Added intervention_phase argument to NAP(), SMD(), and other user-facing effect size calculation functions. 
    * The SMD() function now reports the SD (baseline SD or pooled SD) used in the denominator of the standardized mean difference.
    * The SMD() function now returns a warning instead of an error when the standard deviation is zero.
    * Added option for Kendall's rank correlation in the Tau_BC() function.
    * Added warn argument to Tau_BC() function, to control printing of messages regarding baseline trend test.
* Changes to the SCD_effect_sizes shiny app
    * The batch entry calculator now includes an interface for visualizing data series.
    * The batch entry calculator now includes an interface for taking a subset of observations.
    * Miscellaneous aesthetic and layout fixes.
    * Added unit tests of shiny app results.

# SingleCaseES 0.5.0

* batch_calc_ES() gains an argument aggregate, for post-processing effect size estimates by averaging across the levels of a variable.
* Added functions for calculating two new effect sizes: 
    * log ratio of medians
    * baseline-corrected Tau
* Updates to SCD_effect_sizes() shiny app.
    * Added aggregate feature.
    * Added log ratio of medians and baseline-corrected Tau effect size measures.
    * Allowed use of .xlsx files.
    * R code for replicating the batch calculation.

# SingleCaseES 0.4.4

* Modified LOR, LRRd, and LRRi functions to use truncated sample variances. This ensures that the standard error of the effect size estimate is strictly greater than zero.
* Modified the standard error calculations for NAP and Tau to ensure that they return values strictly greater than zero.
* Modified the Wright2012 example data so that the session numbers align across outcomes.

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
