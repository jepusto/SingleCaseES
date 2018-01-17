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
select "Upload data from a file." By default, the app assumes the first row of the data is the column names. 
If not, uncheck the box. You will need to specify what seperates columns (commas, semicolons, spaces, or tabs), 
as well as whether or not the data includes quotations (it probably will not).

Once you have uploaded a dataset, select the "Estimate" tab. In the left-hand panel, 
specify the column names corresponding to each of the following variables:

- __Series Identifiers__: One or more columns that help to uniquely identify individual
  series. To estimate an effect size for several series within a single study, the series identifier 
  might be a column for case name or pseudonym. To estimate an effect size for several studies that 
  each contained several cases, the series identifiers would need to include a variable listing the 
  source study and a variable with each case's name or pseudonym. If some series had multiple phase
  pairs (such as an ABAB design) and you desire to estimate a separate effect size for each phase 
  pair, you would  need to include a variable specifying phase pair membership.

- __Phase Indicator__: A column of phase indicators. 

- __Baseline Phase Value__ : A selection box containing all unique values in the __Phase Indicator__ column.
  Select the value associated with outcomes in the baseline phase. The calculator assumes that all baseline phases 
  across every series have the same value in the __Phase Indicator__ column.

- __Treatment Phase Value__: A selection box containing all unique values in the __Phase Indicator__ column.
  Select the value associated with outcomes in the treatment phase. The calculator assumes that all treatment phases 
  across every series have the same value in the __Phase Indicator__ column.
  
- __Outcome__: A column of outcomes.
  
- __Within-case session number__: A column that allows the calculator
  to sequentially order each case. For each case, number the first observation 1,
  the second 2, up through the final observation.
  
You Can then select all desired effect sizes and specify details (if relevant): 

- __Direction of Improvement__ : If the desired effect size is a non-overlap index you will
  need to specify the expected direction of improvement. If all of the behaviors in your dataset have a uniform            
  improvement direction, then you may select "all increase" or "all decrease". If the direction of improvement
  varies from series to series, you can select "by series". If you choose "by series", you will need 
  to include a variable in your dataset that specifies "increase" or "decrease" for each series, and
  select that variable name in the drop box that appears when you select "by series".
    
- __Confidence Level__:  For non-overlap effect sizes that have standard errors, as well as all parametric 
  effect sizes you, the calculator will provide confidence intervals. You will need to specify what coverage
  you desire in your confidence interval. The default is 95% confidence intervals.
  
The default format for the output is that each series will have a separate line for each calculated
effect size and any accompanying standard errors and confidenc interval. If you would prefer that each 
series is a single line with separate columns for each calculated value, check the box labelled
__Convert data to wide format.__

effect size and
