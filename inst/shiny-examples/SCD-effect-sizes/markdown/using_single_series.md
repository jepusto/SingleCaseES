# Single-entry calculator

To use the single-case entry calculator we first click on the "Single-Series Calculator" section at the top of the screen.

## Data input

Begin by entering values for the outcome data in the Phase A field (for baseline phase data) and the Phase B field (for treatment phase data). Observations from each time-point can be separated by spaces, commas, tabs, or hard returns. You can also copy and paste directly from a spreadsheet range. Check the "Show graph" box to display a plot of the data you have entered.

## Effect sizes

1. Select __Non-overlap__ or __Parametric__ effect size.

2. Specify values for the following options:

  2.1 __Effect size index:__ Choose your preferred effect size index.
  
  2.2 __Direction of improvement:__ Specify whether therapeutic improvements correspond to increases (e.g. increases in on-task behavior) or decreases (e.g. reductions in the frequency of perseverative behavior) in the outcome measure.
  
  2.3 __Confidence level:__ For effect size indices that have known sampling variances, the calculator will report an approximate confidence interval. Specify your preferred coverage level for the confidence interval. The default is 95% confidence intervals.
  
  2.4 __Digits:__ The number of digits reported for the effect size estimates (and confidence intervals if applicable). For instance, an NAP of 0.85456 will be rounded to 0.855 if you select 3 digits, or to 0.85 if you select 2 digits. 
  
3. If using the Tau-BC index (one of the available __non-overlap__ effect sizes), there are several additional options available:

  3.1 __Choose a method for calculating Tau index:__ Specify a method for calculating the Tau index after adjusting for baseline trend. If you select “Tau (non-overlap)”, then it will be calculated just like it is for the Tau effect size. If you select “Kendall rank correlation,” then it will be calculated using Kendall’s rank correlation with an adjustment for ties (as originally proposed by Tarlow, 2017). 
  
  3.2 __Test for baseline trend:__ Indicate whether to conduct a pre-test for significance of the time trend in the baseline phase. If you select “No,” the outcomes are adjusted for the baseline trend using Theil-Sen regression, and the residuals from Theil-Sen regression are used to calculate the Tau index. If you select “Yes,” then a significance level needs to be specified in the following input. 
  
  3.3 __Significance level for the initial baseline trend test:__ Specify a significance level to use for the baseline trend test. The default is 0.05. If the baseline slope is not significantly different from zero, then no baseline trend adjustment is made, and the Tau-BC effect size is the same as Tau. If the baseline slope is significantly different from zero, then Tau-BC is calculated after adjusting for the trend via Theil-Sen regression.
  
4. If using the SMD effect size (one of the available __parametric__ effect sizes), there is one additional input needed.

  4.1 __Standardize by:__ For the standardized mean difference effect size index, an option will appear allowing you to select whether to calculate it using the standard deviation of the baseline observations (baseline SD) or by the standard deviation pooled across the baseline and treatment phases (pooled SD). 
  
5. If using the LOR, LRRd, or LRR effect sizes (all parametric effect sizes), several further pieces of information are needed.

  5.1 __Outcome Scale:__ This is how the outcome measurements are scaled. Note here that the log odds ratio only accepts data scaled as a percentage (ranging from 0-100) or proportion (ranging from 0-1).
    
    5.1.1 For continuous recording or interval recording, the scale will either be a percentage (ranging from 0% to 100%) or a proportion (ranging from 0 to 1). 
    
    5.1.2 For frequency counting, the scale will either be a count (a raw frequency count) or a rate (events per unit time). 
    
    5.1.3 For other measurement procedures, select “other” for the outcome scale. 
    
  5.2 __Total intervals per session:__ This information is used for calculating a truncation constant for mean levels very near the floor of 0. 
  
      5.2.1 If the outcome data were measured via some interval recording procedure (e.g. momentary time sampling or partial interval recording), the total number of intervals observed in each session. 
      
      5.2.2 If this varies from session to session, use the mean. 
      
      5.2.3 Leave blank if the outcome data were not measured via an interval recording method. 
      
  5.3 __Session length (in minutes)__: The length of the observation sessions in minutes. If the length of the observation session varies, use the mean. If unknown, leave blank. This information is used for calculating a truncation constant for mean levels very near the floor of 0.

  5.4 __User-specified floor constant:__ You may also specify your own truncation constant instead of using the constant calculated based on the information in previous fields.

## Results

The single-series calculator will display an effect size estimate, as well as a standard error, confidence interval, and baseline or pooled SD (if calculating SMD only) if applicable. Check the “Show methods and references” box for further details about the calculation of an effect size index, along with relevant references. 