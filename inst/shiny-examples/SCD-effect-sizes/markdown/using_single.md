# Single-entry Calculator

The single-entry calculator will estimate an effect size using data from a single 
series. To use the single-entry calculator with your own data, you must first enter
your data in the Phase A field and the Phase B field. If you desire, you can check 
the "Show graph" box to display a plot of the data you have entered.

You will then need to specify values for the following effect size options: 

- __Non-overlap__ or __Parametric__

- __Effect Size Index__

- __Direction of Improvement__ : If the desired effect size is a non-overlap index you will
  need to specify the expected direction of improvement.

- __Confidence Level__:  For non-overlap effect sizes that have standard errors, as well as all parametric 
  effect sizes you, the calculator will provide confidence intervals. You will need to specify what coverage
  you desire in your confidence interval. The default is 95% confidence intervals.
  
- __Digits__: The number of significant digits to round to. For instance, an SMD of 95.0456 will be rounded to
  95.046 if the selector is set to 3 digits, or to 95.05 if the selector is set to 2 digits.

- __Standardize by__: If you select the standardized mean difference (SMD) you will be given an option
  to select with variance to standardize the effect by. The baseline SD option will standardize the SMD
  by the variability of the observations in the baseline, and the pooled SD option will standardize the
  SMD by the pooled variance of the two phases.
  
- __Measurement procedure__: If you select the log-response ratio (LRR) you will be given an option to
  select the appropriate measurement procedure. For momentary time sampling or continuous recording data, 
  choose "continuous recording". For partial interval recording or whole interval recording, choose 
  "interval recording." For event counting data, choose "event counting". For measurement procedures not 
  listed, choose "other".
  
- __Outcome Scale__: If you select the log-response ratio (LRR) you will be given an option to select
  the appropriate outcome scale. If the scale of the outcome for the series is not listed, select "other".

- __Log-response ratio floor__: If you select the log-response ratio (LRR) you will be given an option to enter the approriate floor for the LRR. The floor must be a non-negative number.

- __Show methods and references__: By default, the calculator will simply provide estimates and confidence
  intervals (if applicable). More detailed information and relevant formulas will be provided if this box is
  checked.
  
The single-entry calculator will display an effect size estimate, as well as a standard 
error  and confidence interval if applicable. If you are interested in the methods and references
for a given effect size, you can check the "Show methods and references" box for more details.
