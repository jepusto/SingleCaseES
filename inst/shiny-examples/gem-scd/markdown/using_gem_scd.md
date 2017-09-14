### Single-entry Calculator

The single-entry calculator comes populated with data from Thorne and Kamps (2008)
for participant one. Click "Estimate Model" to view example output. 

The single-entry calculator has three entry fields:

- Treatment Assignment: A series of treatment assignment indicators, with 0 
  corresponding to occasions where the treatment WAS NOT applied and 1 
  corresponding to occasions where the treatment WAS applied. Treatment 
  assignment indicators can be seperated by commas, semicolons, tabs, or spaces.

- Outcomes: A series of outcomes of the same length as the treatment assignment
  indicators. If you intend to use the quasi-binomial or binomial variance
  function, and you have data in a percentage format, you should check the box
  that transforms the data to proportions.
  
- m: This is the number of treatment sessions you would like to estimate the
  treatment effect for. Ideally, this number should represent a length of time
  present within the data. For instance, if you had an ABAB design where the
  length of the two B phases was both 5 measurement occasions, it would not be
  good practice to estimate the model with M = 10, because that is much longer
  than the observed data. If you intend to apply this across several cases, pick
  a common value for m that is not longer than most or any of the treatment
  phases found in any of the cases to analyzed.
  
It is also necessary to select a variance function and a link function.

- quasi-binomial/binomial: These variance functions are typically appropriate 
  when outcome data are in proportions or percentages. The quas-binomial 
  variance function is a less restrictive assumption than the binomial variance 
  function and we would generally encourage use of the quasi-binomial over the 
  binomial. The typical link function for these two variance functions is the 
  logit link.

- quasi-Poisson/Poisson: These variance functions are typically appropriate when
  the outcome data are counts or rates (such as behaviors per minute or hour).
  Similar to the quasi-binomial, the quasi-Poisson variance function is a less
  restrictive assumption than the Poisson variance function and we would
  generally encourage the use of the quasi-Poisson over the Poisson. The typical
  link function for these two variance functions is the log link.
  
- guassian: This variance function is typically appropriate for data that are
  assumed to be normally distributed. This is equivalent to the assumption
  made about the error structure in OLS regression. The typical link function
  for this variance function is the identity link.
  
### Batch Entry

The batch calculator includes several example datasets. In order to see example 
output, choose a dataset. The data will automatically populate the data screen. 
Select the "Estimate" tab and all of the correct selections will automatically 
be populated. You can click "Estimate Models" to see example output and then 
download the example output.

In order to use the batch calculator, you will need to save your data in a .csv
or text file that uses semicolons, spaces or tabs as delimiters. By default, the
app assumes the first row of the data is the column names. If not, uncheck the box.
You will need to specify what seperates columns (commas, semicolons, spaces, 
or tabs), as well as whether or not the data includes quotations (it probably
will not).

The data will need to have four columns at minimum:

- Series Identifiers: One or more columns that help to uniquely identify individual
  series. If you are estimating several series within a single study, that might be
  a column for case name or pseudonym. If you were estimating the effects for several
  studies that each contained several cases, you would need to have a column identifying
  which study the case came from, and then a column identifying the case.

- Treatment Assignment: A column of treatment assignment indicators, with 0 
  corresponding to occasions where the treatment WAS NOT applied and 1 
  corresponding to occasions where the treatment WAS applied. Treatment 
  assignment indicators can be seperated by commas, tabs, or spaces.
  
- Outcomes: A column of outcomes. If you intend to use the quasi-binomial or 
  binomial variance function, and you have data in a percentage format, you 
  should check the box that transforms the data to proportions. NOTE: This will
  currently transform all the data in your data file, so you cannot mix
  proportion and percentage data at present.
  
- Within-case session number: A column that allows the calculator
  to sequentially order each case. For each case, number the first observation 1,
  up to n observations.
  
- m: This is the number of treatment sessions you would like to estimate the
  treatment effect for. You should pick a value that represents a treatment
  phase length longer than most of the treatment phases in found in your data,
  and ideally not longer than any of them to avoid extrapolating outside observed
  values. 

It is also necessary to select a variance function and a link function. This will
be used for all series.

- quasi-binomial/binomial: These variance functions are typically appropriate 
  when outcome data are in proportions or percentages. The quas-binomial 
  variance function is a less restrictive assumption than the binomial variance 
  function and we would generally encourage use of the quasi-binomial over the 
  binomial. The typical link function for these two variance functions is the 
  logit link.

- quasi-Poisson/Poisson: These variance functions are typically appropriate when
  the outcome data are counts or rates (such as behaviors per minute or hour).
  Similar to the quasi-binomial, the quasi-Poisson variance function is a less
  restrictive assumption than the Poisson variance function and we would
  generally encourage the use of the quasi-Poisson over the Poisson. The typical
  link function for these two variance functions is the log link.
  
- guassian: This variance function is typically appropriate for data that are
  assumed to be normally distributed. This is equivalent to the assumption
  made about the error structure in OLS regression. The typical link function
  for this variance function is the identity link.
