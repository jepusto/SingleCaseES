### Single-entry Calculator

The single-entry calculator will estimate the gradual effects model using data 
from a single series. It also provides a graphical representation of the data 
and the fitted model. By default, the single-entry calculator comes populated 
with data from Thorne and Kamps (2008) for participant one. 
Click "Estimate Model" to view example output. 

To use the single-entry calculator with your own data, you must first enter 
information in the following three fields:

- __Treatment Assignment__: A series of treatment assignment indicators, with 0 
  corresponding to occasions where the treatment WAS NOT applied and 1 
  corresponding to occasions where the treatment WAS applied. Treatment 
  assignment indicators can be seperated by commas, semicolons, tabs, or spaces.

- __Outcome__: A series of outcomes of the same length as the treatment assignment
  indicators. If you intend to use the quasi-binomial or binomial variance
  function, and you have data in a percentage format, you should check the box
  that transforms the data to proportions.
  
- __m__: The number of treatment sessions for which you would like to estimate the
  treatment effect. This should usually correspond to a length of time
  present within the data. For example, if you had an ABAB design where the
  length of the two B phases was both 5 measurement occasions, it would not be
  good practice to estimate the model with m = 10, because that is much longer
  than any phase in the observed data. If you intend to apply the model across 
  several cases, pick a common value for m that is not longer than most of the 
  treatment phases found in the set of cases to analyzed.
  
After providing this information, you must then specify a variance function and a 
link function in the "Modeling" panel.

- __quasi-binomial/binomial__: These variance functions are appropriate 
  when outcome data are proportions or percentages. The quas-binomial 
  variance function is a less restrictive assumption than the binomial variance 
  function and we would generally encourage use of the quasi-binomial over the 
  binomial. The typical link function for these two variance functions is the 
  logit link.

- __quasi-Poisson/Poisson__: These variance functions are appropriate when
  the outcome data are counts or rates (such as behaviors per minute).
  Similar to the quasi-binomial, the quasi-Poisson variance function is a less
  restrictive assumption than the Poisson variance function and we would
  generally encourage the use of the quasi-Poisson over the Poisson. The typical
  link function for these two variance functions is the log link.
  
- __gaussian__: This variance function is typically appropriate for data that are
  assumed to be normally distributed. This is equivalent to the assumption
  made about the error structure in OLS regression. The typical link function
  for this variance function is the identity link.
  
Once you have selected the variance function and link function, click "Estimate Model" 
to fit the model and examine the results. 