# Batch-entry calculator

The batch-entry calculator will estimate the gradual effects model using data 
from multiple series, distinguished by one or more series identifiers. This is 
useful if you are using the gradual effects model to estimate effect sizes as 
part of a meta-analysis, for instance. 

The batch calculator includes several example datasets. In order to see example 
output, choose a dataset. The data will automatically populate the data screen. 
Select the "Estimate" tab and all of the correct selections will automatically 
be populated. You can click "Estimate Models" to see example output and then 
download the example output.

To use the batch-entry calculator with your own data, you will need to save your data in a .csv
or text file that uses semicolons, spaces or tabs as delimiters. On the "Batch Entry" screen, 
select "Upload data from a file." By default, the app assumes the first row of the data is the column names. 
If not, uncheck the box. You will need to specify what seperates columns (commas, semicolons, spaces, 
or tabs), as well as whether or not the data includes quotations (it probably will not).

Once you have uploaded a dataset, select the "Estimate" tab. In the left-hand panel, 
specify the column names corresponding to each of the following variables:

- __Series Identifiers__: One or more columns that help to uniquely identify individual
  series. To apply the model to several series within a single study, the series identifier might be
  a column for case name or pseudonym. To apply the model to several studies that each contained 
  several cases, the series identifiers would need to include a variable listing the source study 
  and a variable with each case's name or pseudonym.

- __Treatment Assignment__: A column of treatment assignment indicators, with 0 
  corresponding to occasions where the treatment WAS NOT applied and 1 
  corresponding to occasions where the treatment WAS applied.
  
- __Outcome__: A column of outcomes. If you intend to use the quasi-binomial or 
  binomial variance function, and you have data in a percentage format, you 
  should check the box that transforms the data to proportions. NOTE: Currently, 
  checking the box will transform all the data in your data file, so you cannot mix
  proportion and percentage data at present.
  
- __Within-case session number__: A column that allows the calculator
  to sequentially order each case. For each case, number the first observation 1,
  the second 2, up through the final observation.
  
You will then need to specify values for the following modeling options: 

- __m__: The number of treatment sessions for which you would like to estimate the
  treatment effect. You should pick a value that represents a treatment
  phase length longer than most of the treatment phases in found in your data,
  and ideally not longer than any of them to avoid extrapolating outside observed
  values. 

- __Variance function__: The same variance function will be used for all series. The options are:

    - quasi-binomial/binomial: These variance functions are appropriate 
      when outcome data are proportions or percentages. The quas-binomial 
      variance function is a less restrictive assumption than the binomial variance 
      function and we would generally encourage use of the quasi-binomial over the 
      binomial. The typical link function for these two variance functions is the 
      logit link.
    
    - quasi-Poisson/Poisson: These variance functions are appropriate when
      the outcome data are counts or rates (such as behaviors per minute).
      Similar to the quasi-binomial, the quasi-Poisson variance function is a less
      restrictive assumption than the Poisson variance function and we would
      generally encourage the use of the quasi-Poisson over the Poisson. The typical
      link function for these two variance functions is the log link.
      
    - gaussian: This variance function is typically appropriate for data that are
      assumed to be normally distributed. This is equivalent to the assumption
      made about the error structure in OLS regression. The typical link function
      for this variance function is the identity link.


- __Link function__: The link function determines the form of the treatment effect size measure 
estimated by the gradual effects model. The log link function corresponds to a log response ratio
effect size measure; the logit link function corresponds to a log odds ratio; and the identity link 
function corresponds to a raw difference in levels (without standardization). 

Once you have selected the variance function and link function, click "Estimate Models" 
to fit the model and examine the results. Click "Download displayed results" to obtain a 
.csv file with the model estimates.


