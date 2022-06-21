# Using the Multiple-Series Calculator

For video instructions on using the multiple-series calculator, visit <https://www.youtube.com/watch?v=1futNUH7e8k>.

To use the multiple-series entry calculator we first click on the "Multiple-Series Calculator" section at the top of the screen.

## Data

1. Select your data source - either your own data or an example dataset.

2. If using an example dataset, select one of interest from the dropdown menu.

3. If using your own data from a .csv or text file

  3.1 Save your data in a .csv or text file that uses semicolons, spaces, or tabs as delimiters
  
  3.2 Select “Upload data from a .csv or .txt file.” Select “Upload data from a .xlsx file” if you are using a .xlsx file.
  
  3.3 By default, the app assumes the first row of the data contains column names. If this is not true, uncheck the box. 
  
  3.4 Specify the column delimiter (commas, semicolons, spaces, or tabs) used by the data file, and specify whether or not the data includes quotations (it probably won’t). 
  
4. If using your own data from a .xlsx file

  4.1 Save your data in a .xlsx file. Include one dataset per sheet.
  
  4.2 Select the sheet with your data from the drop-down box “Select a sheet”.
  
5. The dataset will be loaded and automatically populated to the data screen.

6. __Filtering variables:__ This provides you the ability to filter by variables within your dataset (e.g. for calculating effect sizes for certain groups of students). The drop-down menu automatically populates with all variables in your uploaded data set. 

  6.1 If selecting a variable, you are asked to __select values for each filtering variable__. This gives a drop-down menu of all unique values for that variable in your data set. From there you can select the values to include in your calculations (e.g. selecting the pseudonyms of the group of students you are interested in). 

## Variables

Once you have uploaded a dataset, select the “Variables” tab in the panel on the left of your screen. 

If you are using an example dataset, the options on this tab will be automatically populated with the correct choices for the selected examples. You can skip to the “Estimate” section. 

If you are using your own dataset, specify the column names from your dataset corresponding to each of the following variables:

1. __Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior)__: One or more columns that uniquely identify each individual series.

  1.1 If your data contains several single-case series from a single primary study, select a column for case name or pseudonym. 
  
  1.2 If your data contains single-case series from several studies, each of which includes several cases, select a variable identifying the source study and a variable identifying each case name or pseudonym.
  
  1.3 If some series had multiple phase pairs (such as an ABAB design) and you want to estimate a separate effect size for each phase pair, you should also include a variable specifying phase pair membership.
  
  1.4 If your series had multiple outcomes and you want to estimate a separate effect size for each outcome, this should also include a variable specifying outcome type. 

2. __Select all variables to average across after calculating effect size estimates__: One or more columns that identify variables to average across after calculating effect size estimates. Effect sizes are calculated separately for each unique value of these variables, and then averaged across those variables to get the aggregated effect size estimates. For example, you may choose to calculate aggregate effect size estimates for ABAB designs. 

3. __Phase indicator__: A column of phase indicators. For example, this may be a column where every entry for a baseline or treatment reintroduction phase has an “A” and every entry for a treatment or return-to-treatment phase has a “B.”

4. __Baseline phase value__: A selection box containing all unique values in the __Phase indicator__ column. Select the value corresponding to baseline phases. The calculator assumes that all baseline phases across every series have the same value.

5. __Treatment phase value__: A selection box containing all unique values in the __Phase indicator__ column, excluding the Baseline phase value. Select the value corresponding to the treatment phase. The calculator assumes that all treatment phases across every series have the same value.

6. __Session Number__: A column that allows the calculator to sequentially order the data from each series. For each series, number the first observation 1, the second 2, up through the final observation.

7. __Outcome__: A column containing the outcome (dependent variable) data from each session.

8. __Direction of improvement__: Specify whether therapeutic improvements correspond to increases (e.g. increases in on-task behavior) or decreases (e.g. reductions in the frequency of perseverative behavior) in the outcome measure.

  8.1 If all of the behaviors in your dataset have the same direction of improvement, then select “all increase” or “all decrease” as appropriate.
  
  8.2 If the direction of improvement varies from series to series, select “by series.” You will then need to include a variable, for __Select variable identifying improvement direction__, in your dataset that specifies “increase” or “decrease” for each series, and select that variable name in the drop-down box that appears when you select “by series.”

## Plot

Once you have completed all selections on the “Variables” tab, select the “Plot” tab in the panel on the left of your screen. The plot tab displays a graph of outcome scores by session number, differentiating (by color) the phases in the data. You are able to customize this graph by making the following selections:

1. __Display plots for each value of this variable__: Here you can specify variables for the plot to be grouped by (i.e. one plot for each participant in the study or for each outcome type). 

2. __Select a value for each grouping variable__: It then allows you to plot certain values for each grouping variable (i.e. which outcome type or participant is shown on the plot).  


## Estimate

Now select the “Estimate” tab and choose any effect size indices by clicking on the available check-boxes. You will need to specify the following:

1. __Weighting Scheme to use for aggregating__: If you specified one or more aggregating variables in the “Variables” tab, an option will appear allowing you to specify a weighting scheme. The default is “equal” which weighs the effect size estimates equally. Other options include:

  1.1 “1/V” which uses the inverse sampling variances of the effect size estimates as the weights for aggregating. 

  1.2 “nA” which uses the number of baseline phase observations as the weights for aggregating.

  1.3 “nB” which uses the number of treatment phase observations as the weights for aggregating.

  1.4 “nA*nB” which uses the product of the number of baseline and treatment phases as the weights for aggregating.

  1.5 “1/nA + 1/nB” which uses the sum of the inverse number of baseline phases and the inverse number of treatment phases as the weights for aggregating.

2. __Confidence level__: For effect size indices that have known sampling variances, the calculator will report approximate confidence intervals. Specify your preferred coverage level for the confidence interval. The default is 95% confidence intervals. 

3. __Long or wide format?__ This option is only relevant if you select more than one effect size metric. If you select long, each effect size estimate (and any accompanying standard errors and confidence interval) will appear in a separate row. If you select wide, each effect size will appear in a separate column, with a single line per data series. 

Certain effect sizes require additional inputs as follows:

1. If you select Tau-BC

  1.1 __Choose a method for calculating Tau index__: Specify a method for calculating the Tau index after adjusting for baseline trend. If you select “Tau (non-overlap)”, then it will be calculated just like it is for the Tau effect size. If you select “Kendall rank correlation,” then it will be calculated using Kendall’s rank correlation with an adjustment for ties (as originally proposed by Tarlow, 2017).
  
  1.2 __Use baseline trend test for Tau-BC?__ If “Always adjusting for baseline trend” the outcomes are adjusted for baseline trend using Theil-Sen regression, and the residuals from Theil-Sen regression are used to calculate the Tau index. If “Pretest for baseline trend, adjust if significant” then a significance level needs to be specified in the following input.
  
  1.3 __Significance level for baseline trend test__: What significance level to use for the baseline trend test. The default is 0.05.

2. If you select SMD

  2.1 __Standardize SMD__: For the standardized mean difference effect size index, an option will appear allowing you to select whether to calculate it using the standard deviation of the baseline observations (baseline SD) or by the standard deviation pooled across the baseline and treatment phases (pooled SD). The selected SD will be reported in the output as long as no aggregation variables are selected. It will not be reported if effect sizes are aggregated. 

3. If you select LOR, LRRd, or LRRi

  3.1 __Outcome Scale__: 
  
    3.1.1 If all of the data series use the same measurement, then select the appropriate outcome scale - “all percentage”, “all proportion”, “all count”, “all rate”, or “all other.”
    
    3.1.2 If the outcome scale varies from series to series, select “by series.” If this is the case, then you will need to include a variable in your dataset that specifies “percentage”, “proportion”, “count”, “rate”, or “other” for each series, and then select the variable name in the drop box that appears.
    
    3.1.3 Note: LRRi and LRRd accept data for all outcome scales while LOR is only calculated for data series that are on a proportion or percentage scale. 
    
  3.2 __Intervals per observation session__: Here you can select a variable in the dataset containing the number of intervals per observation session. For series that were not observed using an interval method, the value for the entry should be left blank. This information is used for calculating a truncation constant for mean values very near the floor of 0.
  
  3.3 __Length of each observation session__: Here you are able to select a variable in the dataset containing the length of each observation session (in minutes). If this value is unknown, it should be set to missing (NA). This information is used for calculating a truncation constant for mean values very near the floor of 0.

  3.4 __Floor for the log-response or log-odds ratio__: Here you are able to specify your own truncation constant instead of using the constant calculated based on the information in previous fields. 

After you have selected one or more effect sizes, the “Estimate” button will appear. 

- Click “Estimate” to view the example output. 

- Click “Download results” to download the effect size estimates as a .csv file

- Click the “Syntax for R” tab to see the automatically generated R code for replicating the results from the multiple-series calculator. You can copy this code into a .R file for reference..