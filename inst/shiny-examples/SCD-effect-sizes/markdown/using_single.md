# Single-entry calculator

The single-entry calculator will estimate an effect size using data from two
phases (i.e., a baseline phase and a treatment phase) within a single series. To
use the single-entry calculator, begin by entering values for the outcome data
in the Phase A field and the Phase B field. Observations from each session can 
be separated by spaces, commas, tabs, or hard returns, or can be copied directly
from a spreadsheet range. 

If you wish, you can check the "Show graph" box to display a plot of the data you
have entered.

You will then need to specify values for the following options: 

- __Non-overlap__ or __Parametric__

- __Effect Size Index__: Choose your preferred effect size index.

- __Direction of Improvement__: Specify whether therapeutic improvements correspond to increases (e.g., increases in on-task behavior) or decreases (e.g., reductions in the frequency of perseverative behavior) in the outcome measure.

- __Confidence Level__:  For effect size indices that have known sampling variances, the calculator will report approximate confidence intervals. Specify your preferred coverage level for the confidence interval. The default is 95% confidence intervals.
  
- __Digits__: The number of digits reported for effect size estimates (and confidence intervals if applicable). For instance, an NAP of 0.95456 will be rounded to 0.955 if you select 3 digits, or to 0.95 if you select 2 digits.

- __Standardize by__: For the standardized mean difference effect size index, an option will appear allowing you select whether to calculate it using the standard deviation of the baseline observations (baseline SD) or by the standard deviation pooled across the baseline and treatment phases (pooled SD).

- __Additional input for the log response ratio__. If you select the log-response ratio (LRRi or LRRd), several further input boxes will appear.

  - __Outcome Scale__: How the outcome measurements are scaled. For continuous recording or interval recording, the scale will either be a percentage (ranging from 0% to 100%) or a proportion (ranging from 0 to 1). For frequency counting, the scale will either be a count (a raw frequency count) or a rate (events per unit time). For other measurement procedures, select "other" for the outcome scale.
  - __Total intervals per session__: If the outcome data were measured via some interval recording procedure (e.g. momentary time sampling or partial interval recording), the total number of intervals observed in each session. If this varies from session to session, use the mean. Leave blank if the outcome data were not measured via an interval recording method. This information is used for calculating a truncation constant for mean levels very near the floor of 0.
  - __Session length (in minutes)__: The length of the observation sessions in minutes. If the length of the observation session varies, use the mean. If unknown, leave blank. This information is used for calculating a truncation constant for mean levels very near the floor of 0.
  - __User-specified floor constant__: You may also specify your own truncation constant instead of using the constant calculated based on the information in previous fields.

    
- __Additional input for the log odds ratio__. If you select the log-odds ratio (LOR), several further input boxes will appear.
  - __Outcome Scale__: How the outcome measurements are scaled. For continuous recording or interval recording, the scale will either be a percentage (ranging from 0% to 100%) or a proportion (ranging from 0 to 1). The log odds ratio only accepts percentage or proportion scaled data.
    - __Total intervals per session__: If the outcome data were measured via some interval recording procedure (e.g. momentary time sampling or partial interval recording), the total number of intervals observed in each session. If this varies from session to session, use the mean. Leave blank if the outcome data were not measured via an interval recording method. This information is used for calculating a truncation constant for mean levels very near the floor of 0.
    - __User-specified floor constant__: You may also specify your own truncation constant instead of using the constant calculated based on the information in previous fields.

The single-entry calculator will display an effect size estimate, as well as a
standard error and a confidence interval if applicable. Check the "Show methods
and references" box for further details about the calculation of an effect size
index, along with relevant references.
