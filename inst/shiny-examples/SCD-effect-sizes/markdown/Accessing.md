# Accessing the calculator

There are two ways to access the SCD effect size calculator: on your own computer, using the RStudio software, or via a website. Since you are reading this now, you have presumably already figured out at least one of these methods. The simplest way to access the calculator is via the web, at <https://jepusto.shinyapps.io/SCD-effect-sizes/>. This version of the calculator is hosted by a service called [shinyapps.io](https://www.shinyapps.io/), which imposes limitations on users and hours spent on the site. Consequently, you might find that the site is not always available, especially towards the end of each month. Running the effect size calculator on your own computer through RStudio requires a bit more effort to get set up, but will allow for extensive use, run faster, and maintain privacy of your data because everything is run locally from your computer. More explicit directions for each method of accessing the calculator are provided below. 

Once you have accessed the calculator using one of these methods, you will need to determine whether the single-series or multiple-series calculator is more appropriate for your data. 

- The __single-series calculator__ estimates effect sizes using data from two phases (i.e. a baseline phase and treatment phase) within a _single_ series.

- The __multiple-series calculator__ estimates effect sizes using data from _multiple_ series, distinguished by one or more series identifier variables. This is useful if you are calculating effect sizes for several participants in a primary study (e.g., four participants from a multiple baseline design) or calculating effect sizes for multiple cases from multiple studies (e.g., for a meta-analysis for findings from single-case research designs).

## Access through the shinyapps.io website

1. Visit <https://jepusto.shinyapps.io/SCD-effect-sizes/>.

2. If using the __single-series calculator__, select the "Single-Series Calculator" tab at the top of your screen. Follow the instructions given in the "Using the single-series calculator" section on the "About" tab.

3. If using the __multiple-series calculator__, select "Multiple-Series Calculator" at the top of your screen. Follow the instructions given in the "Using the multiple-series tab calculator" section on the "About" tab.

## Access through RStudio

To access the effect size calculator on your own computer, you will need to install two pieces of open-source and freely available software (R and RStudio) and follow further steps to configure the software. Details for installation and RStudio access are described below. A video walk-through is available at <https://www.youtube.com/watch?v=Zg-fheKHvw4&t=1s> (Mac) or <https://www.youtube.com/watch?v=ga03oojNbbQ> (Windows).

1. Install R via <http://cran.r-project.org/>

  1.1 Navigate to the "Download and Install R" section and select the relevant download for your computer.
  
  1.2 Download the latest version of R and install it following the instructions given. 

2. If you are a Windows user, install Rtools via <http://cran.r-project.org/bin/windows/Rtools/>
    
3.  Install RStudio from <http://www.rstudio.com/products/rstudio/download/>
    
  3.1 Navigate to the "All Installers" section of the home page and select the relevant download for your computer.
  
  3.2 Follow the installation directions provided. 

4. Once you have all of these programs installed, open RStudio. You will need to install several R packages that are required to run the calculator. Navigate to the console in RStudio (usually in the lower left pane) and type the following commands:

        install.packages("devtools")
        install.packages("sourcetools")
        install.packages("shiny")
        install.packages("markdown")
        install.packages("ggplot2")
        install.packages("rclipboard")
        install.packages("Kendall")
        install.packages("SingleCaseES")

5.  After all packages are installed, type the following commands in the console to start the calculator: 

        library(SingleCaseES)
        SCD_effect_sizes()
        
  The calculator should then open in a new browser window. Once you have gone through the installation process, __*you can skip ahead to this step any time you need to access the calculator*__.

6. If using the __single-series calculator__, select the "Single-Series Calculator" tab at the top of your screen. Follow the instructions given in the "Using the single-series calculator" section on the "About" tab.

7. If using the __multiple-series calculator__, select "Multiple-Series Calculator" at the top of your screen. Follow the instructions given in the "Using the multiple-series tab calculator" section on the "About" tab.

8. To exit the calculator, close the browser tab in which it appears. 
