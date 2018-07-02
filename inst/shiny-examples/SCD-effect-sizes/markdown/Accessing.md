### Accessing the calculator

There are two ways to access this tool: via a website or on your own
computer, using the RStudio software. Since you are reading this now,
you have presumably already figured out at least one of these. Read on
to learn more about both options.

#### Access through the shinyapps.io website

The simplest way to access the calculator is via the web, at
<https://jepusto.shinyapps.io/SCD-effect-sizes/>. This version of the
calculator is hosted by a service called
[shinyapps.io](https://www.shinyapps.io/), which imposes limitations on
the number of concurrent users of the site and the total hours of active use
of the site. Consequently, you might find that the site is not always
available---especially towards the end of the month. If you intend to 
use the calculator extensively, please install it on your own computer 
by following the steps below.

#### Access through RStudio

To run the effect size calculator on your own computer, you
will need to install two pieces of software (R and RStudio), both of
which are open-source and freely available. You will then need to follow
several further steps to configure that software. The installation is
more involved than using the web interface, but it has the benefit of 
letting you run the tool as much as you want, at faster speeds than 
over the web.

1.  Install R from <http://cran.r-project.org/>
2.  For Windows users, install Rtools from
    <http://cran.r-project.org/bin/windows/Rtools/>
3.  Install RStudio from
    <http://www.rstudio.com/products/rstudio/download/>
4.  Once you have these programs installed, you will need to install
    several R packages that are required to run the calculator. Do this
    by typing the following commands at the console prompt:

        install.packages("devtools")
        install.packages("sourcetools")
        install.packages("shiny")
        install.packages("markdown")
        install.packages("ggplot2")
        devtools::install_github("jepusto/SingleCaseES")

5.  After all of these packages are installed, type the following
    commands at the console prompt to start the simulator (the simulator
    should then open in a new window):

        library(SingleCaseES)
        SCD_effect_sizes()

6.  To exit the calculator, close the window in which it appears and
    click the red "Stop" icon in the upper right-hand corner of the
    RStudio console window.
