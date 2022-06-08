# Using the Multiple-Series Calculator

To use the multiple-series entry calculator, click on the "Multiple-Series Calculator" tab at the top of the screen.

## Data

1. Select your data source---either your own data or an example dataset.

2. If you are using an example dataset, select one of interest from the dropdown menu.

3. If you are using your own data from a .csv or text file
    
    3.1 Save your data in a .csv or text file that uses semicolons, spaces, or tabs as delimiters
    
    3.2 Select "Upload data from a .csv or .txt file."
    
    3.3 By default, the app assumes the first row of the data contains column names. If this is not true, uncheck the box. 
    
    3.4 Specify the column delimiter (commas, semicolons, spaces, or tabs) used by the data file, and specify whether or not the data includes quotation marks (it probably won’t). 
  
4. If using your own data from a .xlsx file
    
    4.1 Save your data in a .xlsx file. Include one dataset per sheet.
    
    4.2 Select "Upload data from a .xlsx file."
    
    4.2 Select the sheet with your data from the "Select a sheet" drop-down box.
    
5. The dataset will be loaded and automatically populated to the data screen.

6. __Filtering variables:__ This option allows you to take a subset of the rows of your dataset (e.g. for calculating effect sizes for certain groups of students). The drop-down menu automatically populates with all variables in your uploaded dataset.
    
    6.1 Select one or more variables that determine which rows of your dataset to retain. For example, to include data for only a subset of students, select the variable containing student pseudonyms.
    
    6.2 __Select values for each filtering variable__: For each selected variable, a drop-down menu will appear with all unique values for that variable in your data set. Select the values to retain for further calculations (e.g. select the pseudonyms of the students to keep in the data). 
    
    6.3 The dataset on the right-hand side of the screen will then be modified to include only the rows with values you select. 

## Variables

Once you have uploaded a dataset, select the “Variables” tab in the panel on the left of your screen. 

If you are using an example dataset, the options on this tab will be automatically populated with the correct choices for the selected examples. You can skip to the "Plot" or “Estimate” section. 

If you are using your own dataset, choose the column names from your dataset corresponding to each of the following variables:

1. __Select all variables uniquely identifying cases (e.g. pseudonym, study, behavior)__: One or more columns that uniquely identify each individual series.
    
    1.1 If your data contains several single-case data series from a single primary study, select a column for case name or pseudonym. 
    
    1.2 If your data contains single-case data series from several studies, each of which includes several cases, select a variable identifying the source study and a variable identifying each case name or pseudonym.
    
    1.3 If some series had multiple phase pairs (as in an ABAB design) and you want to estimate a separate effect size for each phase pair, you should also include a variable specifying phase pair membership.
    
    1.4 If your data include multiple outcomes and you want to estimate a separate effect size for each outcome, you should also include a variable specifying outcome type. 

2. __Select all variables to average across after calculating effect size estimates__: One or more columns that identify variables to average across after calculating effect size estimates. Effect sizes are calculated separately for each unique value of these variables, and then averaged across those variables to get the aggregated effect size estimates. For example, you may want to calculate aggregate effect size estimates by averaging across the effects for each phase pair in an ABAB design. In this case, you should include a variable specifying phase pair membership here (but do not include it in the first drop-down box).

3. __Phase indicator__: A column of phase indicators. For example, this may be a column where every entry for a baseline or treatment reintroduction phase has an “A” and every entry for a treatment or return-to-treatment phase has a “B.”

4. __Baseline phase value__: A selection box containing all unique values in the __Phase indicator__ column. Select the value corresponding to baseline phases. The calculator assumes that all baseline phases are labeled with the same value across every included data series.

5. __Treatment phase value__: A selection box containing all unique values in the __Phase indicator__ column, excluding the selected baseline phase value. Select the value corresponding to the treatment phase. The calculator assumes that all treatment phases are labeled with the same value across every included data series.

6. __Session Number__: A column that allows the calculator to sequentially order the data from each series. For each data series, number the first observation 1, the second 2, up through the final observation.

7. __Outcome__: A column containing the outcome (dependent variable) data from each session.

8. __Direction of improvement__: Specify whether therapeutic improvements correspond to increases (e.g. increases in on-task behavior) or decreases (e.g. reductions in the frequency of perseverative behavior) in the outcome measure.
    
    8.1 If all of the behaviors in your dataset have the same direction of improvement, then select “all increase” or “all decrease” as appropriate.
    
    8.2 If the direction of improvement varies from series to series, select “by series.” You will then need to include a variable in your dataset that specifies “increase” or “decrease” for each series. Select this variable name in the  __Select variable identifying improvement direction__ drop-down box that appears when you select "by series."

## Plot

Once you have completed all selections on the “Variables” tab, select the “Plot” tab in the panel on the left of your screen. The plot tab displays a graph of outcome scores by session number, differentiating (by color) the phases in the data. You can customize this graph by making the following selections:

1. __Display plots for each value of this variable__: Here you can specify a variable to display a grouped plot, with one row for each unique level of the selected variable (e.g., one plot for each participant in the study or for each outcome type). 

2. __Select a value for each grouping variable__: For each grouping variable that you selected on the "Variables" tab (under _Select all variables uniquely identifying cases_ or _Select all variables to average across after calculating effect size estimates_), there will be a drop-down box listing each unique value of the variable. Select the set of values for which you would like to view a plot (i.e. which outcome type or participant is shown on the plot).  

## Estimate

Now select the “Estimate” tab. You will need to specify the following:

1. Start by choosing one or more effect size indices by clicking on the available check-boxes. 

2. __Weighting Scheme to use for aggregating__: If you specified one or more aggregating variables in the “Variables” tab, an option will appear allowing you to specify a weighting scheme. The default is "equal," which weighs the effect size estimates equally. Other options include:
    
    2.1 "1/V" uses the inverse sampling variances of the effect size estimates as the weights for aggregating. 
    
    2.2 "nA" uses the number of baseline phase observations as the weights for aggregating.
    
    2.3 "nB" uses the number of treatment phase observations as the weights for aggregating.
    
    2.4 "nA*nB" uses the product of the number of baseline and treatment phases as the weights for aggregating.
    
    2.5 "1/nA + 1/nB" uses the sum of the inverse number of baseline phases and the inverse number of treatment phases as the weights for aggregating.

3. __Confidence level__: For effect size indices that have known sampling variances, the calculator will report approximate confidence intervals. Specify your preferred coverage level for the confidence interval. The default is 95% confidence intervals. 

4. __Long or wide format?__ This option is only relevant if you select more than one effect size index. If you select long, each effect size estimate (and any accompanying standard errors and confidence interval) will appear in a separate row. If you select wide, each effect size will appear in a separate column, with a single line per data series. 

5.  If you select the __Tau-BC__ index (one of the available non-overlap effect sizes), several additional options will appear:

    5.1 __Choose a method for calculating Tau index__: Specify a method for calculating the Tau index after adjusting for baseline trend. If you select “Tau (non-overlap)”, then it will be calculated just as it is for the Tau effect size. If you select “Kendall rank correlation,” then it will be calculated using Kendall’s rank correlation with an adjustment for ties (as originally proposed by Tarlow, 2017).
    
    5.2 __Use baseline trend test for Tau-BC?__ If “Always adjusting for baseline trend” the outcomes are adjusted for baseline trend using Theil-Sen regression, and the residuals from Theil-Sen regression are used to calculate the Tau index. If “Pretest for baseline trend, adjust if significant” then a significance level needs to be specified in the following input.
    
    5.3 __Significance level for baseline trend test__: What significance level to use for the baseline trend test. The default is 0.05.

6. If using the __SMD__ effect size (one of the available parametric effect sizes), one additional input is needed:
    
    6.1 __Standardize SMD__: For the standardized mean difference effect size index, an option will appear allowing you to select whether to calculate the denominator using the standard deviation of the baseline observations (baseline SD) or using the standard deviation pooled across the baseline and treatment phases (pooled SD). The selected SD will be reported in the output as long as no aggregation variables are selected. It will not be reported if effect sizes are aggregated. 

7. If using the __LOR__, __LRRd__, or __LRRi__ effect sizes (all parametric effect sizes), several further pieces of information are needed. This information is used for calculating a truncation constant for mean levels very near the floor of 0. 
    
    7.1 __Outcome Scale__: 
  
    7.1.1 If all of the data series use the same measurement, then select the appropriate outcome scale - “all percentage”, “all proportion”, “all count”, “all rate”, or “all other.”
    
    7.1.2 If the outcome scale varies from series to series, select “by series.” If this is the case, then you will need to include a variable in your dataset that specifies “percentage”, “proportion”, “count”, “rate”, or “other” for each series, and then select the variable name in the drop box that appears.
    
    7.1.3 Note: LRRi and LRRd accept data for all outcome scales while LOR is only calculated for data series that are on a proportion or percentage scale. 
    
    7.2 __Intervals per observation session__: Here you can select a variable in the dataset containing the number of intervals per observation session. For series that were not observed using an interval method, the value for the entry should be left blank. This information is used for calculating a truncation constant for mean values very near the floor of 0.
    
    7.3 __Length of each observation session__: Here you are able to select a variable in the dataset containing the length of each observation session (in minutes). If this value is unknown, it should be set to missing (NA). This information is used for calculating a truncation constant for mean values very near the floor of 0.
    
    7.4 __Floor for the log-response or log-odds ratio__: Here you are able to specify your own truncation constant instead of using the constant calculated based on the information in previous fields. 

8. After you have selected one or more effect sizes, the “Estimate” button will appear. Click “Estimate” to view the calculated effect sizes and associated information. 

9. Click “Download results” to download the effect size estimates as a .csv file

## Syntax for R

Click the “Syntax for R” tab to see automatically generated R code. This code can be used to exactly replicate the results from the multiple-series calculator. In other words, running this code in R should produce results identical to those displayed on the "Estimate" tab. You can copy this code into a .R file for reference.
