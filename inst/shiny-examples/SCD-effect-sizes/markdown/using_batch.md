# Batch-entry calculator

The batch-entry calculator will estimate single-case effect sizes using data
from multiple series, distinguished by one or more series identifiers. This is
useful if you need to estimate effect sizes for a systematic review or
meta-analysis of single-case research.

## Example datasets

The batch calculator includes several example datasets that illustrate how to
structure input data files and carry out the batch calculations. To use an
example dataset, first choose a dataset from the example selector. The dataset
will be loaded and automatically populated to the data screen. Select the
"Calculate" tab and the options fields will be automatically populated with the
correct choices for the selected example. Choose any number of effect size
indexes by clicking on the available check-boxes. After you have selected one or
more effect sizes, the "Calculate" button will appear. Click "Calculate" to view
example output. You can Click the "Download results" button to download the
effect size estimates as a .csv file.

## Using your own data

To use the batch-entry calculator with your own data, you will need to save your
data in a .csv or text file that uses semicolons, spaces, or tabs as delimiters.
On the "Batch Entry" screen, select "Upload data from a file." By default, the
app assumes the first row of the data is the column names. If this is not true,
uncheck the box. Specify the column delimiter (commas, semicolons, spaces, or
tabs) used by the data file, and specify whether or not the data includes
quotations (it probably will not). The dataset will be loaded and automatically
populated to the data screen.

Once you have uploaded a dataset, select the "Variables" tab. In the left-hand
panel, specify the column names from your dataset corresponding to each of the
following variables:

- __Series Identifiers__: One or more columns that uniquely identify each
  individual series. If your data contains several single-case series from a
  single study, the series identifier should be a column for case name/pseudonym.
  If your data contains single-case series from several studies, each of which
  includes several cases, the series identifiers should include a variable
  identifying the source study and a variable identifying each case name/
  pseudonym. If some series had multiple phase pairs (such as an ABAB design) and
  you wanted to estimate a separate effect size for each phase pair, the series
  identifiers should include a variable specifying phase pair membership.

- __Phase Indicator__: A column of phase indicators. For example, this might be
  a column where every entry for a baseline or return-to-baseline phase had an "A"
  and every entry for a treatment phase had a "B."

- __Baseline Phase Value__ : A selection box containing all unique values in the
  __Phase Indicator__ column. Select the value corresponding to baseline phases.
  The calculator assumes that all baseline phases across every series have the
  same value.

- __Treatment Phase Value__ : A selection box containing all unique values in
  the __Phase Indicator__ column, excluding the __Baseline Phase Value__. Select
  the value corresponding to the treatment (intervention) phase. The calculator
  assumes that all intervention phases across every series have the same value.

- __Within-Case Session Number__: A column that allows the calculator to
  sequentially order each case. For each case, number the first observation 1, the
  second 2, up through the final observation.
  
- __Outcome__: A column containing the outcome data from each session.
  
- __Direction of improvement__ : Specify whether therapeutic improvements
  correspond to increases (e.g., increases in on-task behavior) or decreases
  (e.g., reductions in the frequency of perseverative behavior) in the outcome
  measure. If all of the behaviors in your dataset have the same direction of
  improvement, then select "all increase" or "all decrease" as appropriate. If the
  direction of improvement varies from series to series, select "by series." If
  you choose "by series", you will need to include a variable in your dataset that
  specifies "increase" or "decrease" for each series, and select that variable
  name in the drop box that appears when you select "by series".
  
After specifying these details about your data, select the "Estimate" tab. In
the left-hand panel, select one or more effect sizes to calculate on each data
series. If you select certain parametric effect sizes, several additional
options will appear.

- __Standardize by__: If you select the standardized mean difference (SMD), an
  option will appear allowing you select whether to calculate it using the
  standard deviation of the baseline observations (baseline SD) or by the standard
  deviation pooled across the baseline and treatment phases (pooled SD).
  
- __Outcome Scale__: If you select the log-response ratio (LRRi or LRRd) or the
  log-odds ratio, an option will appear allowing you to select the scale on which
  the outcomes are measured. If all of the data series use the same measurement
  scale, then you can select the appropriate outcome scale -- "all percentage",
  "all proportion", "all count", "all rate", or "all other." If the outcome scale
  varies from series to series, select "by series." If you choose "by series" you
  will need to include a variable in your dataset that specifies "percentage",
  "proportion", "count", "rate", or "other" for each series, and then select the
  variable name in the drop box that appears when you select "by series." LRRi and
  LRRd accept data for all outcome scales. LOR is only calculated for data series
  that are on a proportion or percentage scale.

- __Intervals per observation session__: If you select the log-response ratio
  (LRRi or LRRd) or the log-odds ratio, an option will appear allowing you to
  select a variable in the dataset containing the number of intervals per
  observation session. For series that were not observed using an interval method,
  the values for this entry should be left blank. This information is used for
  calculating a truncation constant for mean levels very near the floor of 0.

- __Observation session length__: If you select the log-response ratio (LRRi or
  LRRd), an option will appear allowing you to select a variable in the dataset
  containing the length of each observation session (in minutes). For any data
  where this value is unknown, it should be set to missing (NA). This information
  is used for calculating a truncation constant for mean levels very near the
  floor of 0.
  
- __Log-response or log-ratio ratio floor__: If you select the log-response
  ratio or the log-odds ratio, an option will appear allowing you to specify your
  own truncation constant instead of using the constant calculated based on the
  information in previous fields.
    
    
- __Confidence Level__: For effect size indices that have known sampling
  variances, the calculator will report approximate confidence intervals. Specify
  your preferred coverage level for the confidence interval. The default is 95%
  confidence intervals.

- __Long or wide format?__: If you select long, each effect size estimate (and
  any accompanying standard errors and confidence interval) will appear in a
  separate row. If you select wide, each effect size estimate will appear in a
  separate column, with a single line per data series.
  
After you have selected one or more effect sizes, the "Calculate" button will
appear. Click "Calculate" to view example output. 

Click the "Download results" button to download the effect size estimates as a .csv file.
