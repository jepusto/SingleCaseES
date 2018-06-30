# Single-entry Calculator

The single-entry calculator will estimate an effect size using data from two phases 
(i.e., a baseline phase and a treatment phase) within a single series. To use the 
single-entry calculator with your own data, you must first enter your data in the 
Phase A field and the Phase B field. If you wish, you can check 
the "Show graph" box to display a plot of the data you have entered.

You will then need to specify values for the following options: 

- __Non-overlap__ or __Parametric__

- __Effect Size Index__: Choose your preferred effect size index.

- __Direction of Improvement__: Specify whether therapeutic improvements correspond to increases (e.g., increases in on-task behavior) or decreases (e.g., reductions in the frequency of perseverative behavior) in the outcome measure.

- __Confidence Level__:  For effect size indices that have known sampling variances, the calculator will provide confidence intervals. Specify your preferred coverage level for the confidence interval. The default is 95% confidence intervals.
  
- __Digits__: The number of significant digits reported for effect size estimates (and confidence intervals if applicable). For instance, an NAP of 0.95456 will be rounded to 0.955 if the selector is set to 3 digits, or to 0.95 if the selector is set to 2 digits.

- __Standardize by__: If you select the standardized mean difference (SMD), an option will appear allowing you select whether to calculate it using the standard deviation of the baseline observations (baseline SD) or by the standard deviation pooled across the baseline and treatment phases (pooled SD).

- __Additional input for the log response ratio__. If you select the log-response ratio (LRRi or LRRd), several further input boxes will appear.

  - __Outcome Scale__: How the outcome measurements are scaled. For continuous recording or interval recording, the scale will either be a percentage (ranging from 0% to 100%) or a proportion (ranging from 0 to 1). For event counting, the scale will either be a count (a raw frequency count) or a rate (events per unit time). For other measurement procedures, select "other" for the outcome scale.
    - __Total intervals per session__: If the data were observed via some interval recording method (e.g. momentary time sampling or partial interval recording), the total number of intervals observed in each session. If this varies from session to session, use the mean. Leave blank if the data were not observed via an interval recording method. Used for calculating a truncation constant for values very near the floor of 0.
    - __Observation session length__: The length of the observation sessions in minutes. If the length of the observation session varies, use the mean. If unknown, leave blank. Used for calculating a truncation constant for values very near the floor of 0.
    - __Log-response ratio floor__: The user may also specify their own truncation constant for values very near the floor of zero, if desired.
    
- __Additional input for the log odds ratio__. If you select the log-response ratio (LOR), several further input boxes will appear.
  - __Outcome Scale__: How the outcome measurements are scaled. For continuous recording or interval recording, the scale will either be a percentage (ranging from 0% to 100%) or a proportion (ranging from 0 to 1). The log odds ratio only accepts percentage or proportion scaled data.
    - __Total intervals per session__: If the data were observed via some interval recording method (e.g. momentary time sampling or partial interval recording), the total number of intervals observed in each session. If this varies from session to session, use the mean. Leave blank if the data were not observed via an interval recording method. Used for calculating a truncation constant for values very near the floor of 0.
    - __Log-odds ratio floor__: The user may also specify their own truncation constant for values very near the floor of zero, if desired.

The single-entry calculator will display an effect size estimate, as well as a standard 
error and a confidence interval if applicable. If you are interested in the methods and references
for a given effect size, you can check the "Show methods and references" box for more details.
