# Batch-entry calculator

The batch-entry calculator will estimate single-case effect sizes using data 
from multiple series, distinguished by one or more series identifiers. This is 
useful if you are using SingleCaseES package to estimate effect sizes as 
part of a meta-analysis or other synthetic review.

The batch calculator includes several example datasets. In order to see example 
output, first choose a dataset from the example selector. The dataset will be loaded
and automatically populated to the data screen. Select the "Calculate" tab and the
correct choices for the example dataset will automatically populate all the fields.
Choose any number of effect size check boxes, and the "Calculate" button will appear. 
Click "Calculate"  to view example output, and you can  download the example output 
using the "Download displayed results" button.

To use the batch-entry calculator with your own data, you will need to save your 
data in a .csv or text file that uses semicolons, spaces or tabs as delimiters. On the "Batch Entry" screen, 
select "Upload data from a file." By default, the app assumes the first row of the data is the column names. 
If not, uncheck the box. You will need to specify what separates columns (commas, semicolons, spaces, or tabs), 
as well as whether or not the data includes quotations (it probably will not).

Once you have uploaded a dataset, select the "Calculate" tab. In the left-hand panel, 
specify the column names corresponding to each of the following variables:

- __Series Identifiers__: One or more columns that uniquely identify each individual series. To 
  estimate an effect size for several series within a single study, the series identifier 
  might be a column for case name or pseudonym. To estimate an effect size for several studies that 
  each contained several cases, the series identifiers would need to include a variable listing the 
  source study and a variable with each case name or pseudonym. If some series had multiple phase
  pairs (such as an ABAB design) and you wanted to estimate a separate effect size for each phase 
  pair, you would need to include a variable specifying phase pair membership.

- __Phase Indicator__: A column of phase indicators. For example, this might be a column where every entry for a
  baseline or return-to-baseline phase had an "A" and every entry for a treatment phase had a "B".

- __Baseline Phase Value__ : A selection box containing all unique values in the __Phase Indicator__ column.
  Select the value associated with outcomes in the baseline phase. The calculator assumes that all baseline phases 
  across every series have the same value in the __Phase Indicator__ column.

- __Treatment Phase Value__: A selection box containing all unique values in the __Phase Indicator__ column.
  Select the value associated with outcomes in the treatment phase. The calculator assumes that all treatment phases
  across every series have the same value in the __Phase Indicator__ column.
  
- __Within-Case Session Number__: A column that allows the calculator to sequentially order each case. 
  For each case, number the first observation 1, the second 2, up through the final observation.
  
- __Outcome__: A column of outcomes.
  
- __Direction of improvement__ : If the desired effect size is a non-overlap index you will
  need to specify the expected direction of improvement. If all of the behaviors in your dataset have a uniform
  improvement direction, then you may select "all increase" or "all decrease". If the direction of improvement
  varies from series to series, you can select "by series". If you choose "by series", you will need 
  to include a variable in your dataset that specifies "increase" or "decrease" for each series, and
  select that variable name in the drop box that appears when you select "by series".
  
You can then select all desired effect sizes and specify relevant details: 

- __Standardize by__: If you select the standardized mean difference (SMD) you will be given an option
  to select with variance to standardize the effect by. The baseline SD option will standardize the SMD
  by the variability of the observations in the baseline, and the pooled SD option will standardize the
  SMD by the pooled variance of the two phases.
  
  
- __Outcome Scale__: If you select log-response ratio (LRRi or LRRd) or the log-odds ratio you will be given an option to select
  the appropriate outcome scale. If the measurement procedure is uniform for all of the behaviors in your 
  dataset, then you can select the appropriate outcome scale -- "all percentage", "all proportion", "all count", 
  "all rate", or "all other". If the outcome scale varies from series to series, you can select "by series".
  If you choose "by series" you will need to include a variable in your dataset that specifies "percentage",
  "proportion", "count", "rate", or "other" for each series, and then select the variable name in the drop
  box that appears when you select "by series." LRRi and LRRd accept data for all outcome scales. Data that is specified as something other than proportion or percentage will not have a LOR calculated for it.

- __Intervals per observation session__: If you select log-response ratio or the log-odds ratio you will be given an option to select a variable in your dataset containing the number of intervals per observation session for any data that was observed using an interval method, for example partial interval recording. For any series that were not observed using an interval method, the values for this entry should be set to missing (NA).

- __Observation session length__: If you select the log-response ratio, you will be given an option to select a variableble in your dataset containing the length of each observation session. For any data where this value is unknown, it should be set to missing (NA).
  
- __Log-response or log-ratio ratio floor__: If you select the log-response ratio or the log-odds ratio you will be given an option to enter the approriate floor for the effect size. The floor must be a non-negative number.
    
- __Confidence Level__:  For non-overlap effect sizes that have standard errors, as well as all parametric 
  effect sizes you select, the calculator will provide confidence intervals. You will need to specify what coverage
  you desire in your confidence interval. The default is 95% confidence intervals.

- __Long or wide format?__: If you select long each series will have a separate line for each estimated
  effect size and any accompanying standard errors and confidence interval. If you select wide, each series
  will have a single line with separate columns for each estimated value.
  
Once you have selected all of the desired effect sizes, click on "Estimate" to estimate the effect sizes. When you have estimated effect sizes, you can download the results by clicking on the "Download results" button. 
