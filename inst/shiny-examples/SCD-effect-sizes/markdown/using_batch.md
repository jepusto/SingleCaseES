# Batch-entry calculator

The batch-entry calculator will estimate single-case effect sizes using data 
from multiple series, distinguished by one or more series identifiers. This is 
useful if you are using SingleCaseES package to estimate effect sizes as 
part of a meta-analysis, for instance. 

The batch calculator includes several example datasets. In order to see example 
output, choose a dataset. The data will automatically populate the data screen. 
Select the "Estimate" tab and there will be a button to automatically populate
the correct selections. You can click "Estimate" to see example output and then 
download the example output.

To use the batch-entry calculator with your own data, you will need to save your data in a .csv
or text file that uses semicolons, spaces or tabs as delimiters. On the "Batch Entry" screen, 
select "Upload data from a file." By default, the app assumes the first row of the data is the column names. If not, uncheck the box. You will need to specify what seperates columns (commas, semicolons, spaces, or tabs), as well as whether or not the data includes quotations (it probably will not).

Once you have uploaded a dataset, select the "Estimate" tab. In the left-hand panel, 
specify the column names corresponding to each of the following variables:

- __Series Identifiers__: One or more columns that help to uniquely identify individual
  series. To estimate an effect size for several series within a single study, the series identifier 
  might be a column for case name or pseudonym. To estimate an effect size for several studies that 
  each contained several cases, the series identifiers would need to include a variable listing the 
  source study and a variable with each case's name or pseudonym. If some series had multiple phase
  pairs (such as an ABAB design) and you desire to estimate a separate effect size for each phase 
  pair, you would  need to include a variable specifying phase pair membership.

- __Phase Indicator__: A column of phase indicators. The batch calculator assumes that every series
  begins in the "A" phase, and that any data within a series with after the assumed "A" phase is part
  of the "B" phase.
  
- __Outcome__: A column of outcomes.
  
- __Within-case session number__: A column that allows the calculator
  to sequentially order each case. For each case, number the first observation 1,
  the second 2, up through the final observation.
  
You will then need to specify values for the following effect size options: 

- __Non-overlap__ or __Parametric__

- __Effect Size Index__

- __Direction of Improvement__ : If the desired effect size is a non-overlap index you will
  need to specify the expected direction of improvement. If all of the behaviors in your dataset have a uniform            improvement direction, then you may select "all increase" or "all decrease". If the direction of improvement
  varies from series to series, you can select "by series". If you choose "by series", you will need 
  to include a variable in your dataset that specifies "increase" or "decrease" for each series, and
  select that variable name in the drop box that appears when you select "by series".
    
- __Confidence Level__:  For non-overlap effect sizes that have standard errors, as well as all parametric 
  effect sizes you, the calculator will provide confidence intervals. You will need to specify what coverage
  you desire in your confidence interval. The default is 95% confidence intervals.
