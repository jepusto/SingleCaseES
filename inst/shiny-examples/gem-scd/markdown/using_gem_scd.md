## The Calculator

The calculator is a graphical interface for the `gem_scd` and the `gem_scd_batch` function from the `SingleCaseES` package. Each function has a respective calculator associated with it: the single entry-calculator and the batch-entry calculator.

The single-entry calculator is useful for estimating a small number of cases, a single case at a time. The single-entry calculator also visualizes the entered data and the predicted values from the model, along with the effect size estimate. The estimates of the model will need to be manually copied and pasted from the output window.

The batch-entry calculator is useful when you have data from many cases organized into a single dataset, such as .csv file. The calculator assumes that each observation is is a separate entry, and that sets of observations (individual cases) are uniquely identifiable with one ID variable or a combination of ID variables (such as a Study ID and Participant ID). After the data is uploaded, the batch calculator will estimate the baseline, treatment effect, delay parameter, all the associated standard errors, and the dispersion parameter for each case. The results will be displayed as a table, and also be available for download as a .csv file.

### Single-entry Calculator

The single-entry calculator comes populated with data from Thorne and Kamps (2008), a study on the effects of a group contingency intervention on problem behavior and academic engagement. The pre-populated data is the number of inappropriate verbalizations performed by Participant One during a set of 15 minute observations sessions. Click "Estimate Model" to view example output. 

To use the single entry calculator, you will must fill three fields:

- Treatment Assignment: A series of treatment assignment indicators, with 0 
  corresponding to occasions where the treatment WAS NOT applied and 1 
  corresponding to occasions where the treatment WAS applied. Treatment 
  assignment indicators can be seperated by commas, semicolons, tabs, or spaces.

- Outcomes: A series of outcomes of the same length as the treatment assignment
  indicators. If you intend to use the quasi-binomial or binomial variance
  function, and you have data in a percentage format, you should check the box
  that transforms the data to proportions.
  
- m: This is the number of treatment sessions at which you would like to estimate 
  the treatment effect. Ideally, this number should represent a length of time
  present within the data. For instance, if you had an ABAB design where the
  length of the two B phases was both 5 measurement occasions, it would not be
  good practice to estimate the model with M = 10, because that is much longer
  than the observed data. If you intend to apply this across several cases, pick
  a common value for m that is less than or equal to the number of observation
  sessions in most or all of the treatment phases.
  
You must also select a variance function and a link function.

- quasi-binomial/binomial: These variance functions are typically appropriate 
  when outcome data are in *proportions* or *percentages*. The quas-binomial 
  variance function is a less restrictive assumption than the binomial variance 
  function and we would generally encourage use of the quasi-binomial over the 
  binomial. The typical link function for these two variance functions is the 
  logit link.

- quasi-Poisson/Poisson: These variance functions are typically appropriate when
  the outcome data are *counts* or *rates* (such as behaviors per minute or hour).
  Similar to the quasi-binomial, the quasi-Poisson variance function is a less
  restrictive assumption than the Poisson variance function and we would
  generally encourage the use of the quasi-Poisson over the Poisson. The typical
  link function for these two variance functions is the log link.
  
- Guassian: This variance function is typically appropriate for data that are
  assumed to be normally distributed. This is equivalent to the assumption
  made about the error structure in OLS regression. The typical link function
  for this variance function is the identity link.
  
The selection of a link function is an important consideration that impacts the
effect size produced by the model. The logit link will produce a log-prevalence
odds ratio. The prevalence odds is the proportion of time a behavior occurs
divided by the proportion of time the behavior does not occur. The prevalence 
odds ratio is the proportionate change in the odds as a result of treatment, 
and the log-prevalence odds ratio is the natural logarithm of that quantity.

The log link produces a log-response ratio, or the natural logarithm of the
proportionate change in the outcome as a result of treatment.

The Guassian variance function produces an additive treatment effect, equivalent
to treatment effects produces by typical OLS regression.
  
### Batch Entry

The batch calculator includes several example datasets. In order to see example 
output, choose a dataset. The data will automatically populate the data screen. 
Select the "Estimate" tab and all of the correct selections will automatically 
be populated. You can click "Estimate Models" to see example output and then 
download the example output.

In order to use the batch calculator with your own data, you will need 
to save your data in a .csv or text file that uses semicolons, spaces or tabs as 
delimiters. By default, the app assumes the first row of the data is the column 
names. If not, uncheck the box. You will need to specify what seperates columns 
(commas, semicolons, spaces, or tabs), as well as whether or not the data 
includes quotations (it probably will not).

You will need to organize your data with four columns at minimum:

- Series Identifiers: One or more columns that uniquely identify individual
  series. If you are estimating the model for several series within a single 
  study, that might be a column for case name or pseudonym. 
  If you were estimating the effects for several studies that each contained several 
  cases, you would need to have a column identifying which study the case came 
  from, and then a column identifying the case.

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

Additionally, you will need to determine:
  
- m: This is the number of treatment sessions at which you would like to estimate the
  treatment effect for. You should pick a value that represents a treatment
  phase length longer than most of the treatment phases in found in your data,
  and ideally not longer than any of them to avoid extrapolating outside observed
  values. 

As with the single-entry calculator, you will need to select a variance and link
function:

- quasi-binomial/binomial: These variance functions are typically appropriate 
  when outcome data are in *proportions* or *percentages*. The quas-binomial 
  variance function is a less restrictive assumption than the binomial variance 
  function and we would generally encourage use of the quasi-binomial over the 
  binomial. The typical link function for these two variance functions is the 
  logit link.

- quasi-Poisson/Poisson: These variance functions are typically appropriate when
  the outcome data are *counts* or *rates* (such as behaviors per minute or hour).
  Similar to the quasi-binomial, the quasi-Poisson variance function is a less
  restrictive assumption than the Poisson variance function and we would
  generally encourage the use of the quasi-Poisson over the Poisson. The typical
  link function for these two variance functions is the log link.
  
- guassian: This variance function is typically appropriate for data that are
  assumed to be normally distributed. This is equivalent to the assumption
  made about the error structure in OLS regression. The typical link function
  for this variance function is the identity link.

The selection of a link function is an important consideration that impacts the
effect size produced by the model. The logit link will produce a log-prevalence
odds ratio. The prevalence odds is the proportion of time a behavior occurs
divided by the proportion of time the behavior does not occur. The prevalence 
odds ratio is the proportionate change in the odds as a result of treatment, 
and the log-prevalence odds ratio is the natural logarithm of that quantity.

The log link produces a log-response ratio, or the natural logarithm of the
proportionate change in the outcome as a result of treatment.

The Guassian variance function produces an additive treatment effect, equivalent
to treatment effects produces by typical OLS regression.