# Accessing gem_scd

There are two ways to access this app: via a website or on your own
computer, using the RStudio software. Since you are reading this now,
you have presumably already figure out at least one of these. Read on to
learn more about both options.

#### Access through the shinyapps.io website

The simplest way to access the app is via the web, at
<https://jepusto.shinyapps.io/gem-scd/>. This version is hosted by a
service called [shinyapps.io](https://www.shinyapps.io/), which imposes
limitations on the number of concurrent users of the site and total
hours of active use of the site. Consequently, you might find that the
site is not always available. If you intend to use the tool extensively,
we would encourage you to follow the steps below in order to install it
on your own computer. Doing so has the further advantage that it will
tend to run faster on your own machine than it does over the web.

#### Access through RStudio

In order to run the app on your own computer, you will need to install
two pieces of software (R and RStudio), both of which are open-source
and freely available. You will then need to follow several further steps
to configure that software. The installation is more involved, but has
the benefit of letting you run the simulator as much as you want, at
faster speeds than over the web. Feel free to contact us if you have
trouble following the steps below.

1.  Install R from <http://cran.r-project.org/>
2.  Install RStudio from
    <http://www.rstudio.com/products/rstudio/download/>
3.  Once you have these programs installed, you will need to install
    several R packages that are required to run ARPsimulator. Do this by
    typing the following commands at the console prompt:

        install.packages("ggplot2")
        install.packages("shiny")
        install.packages("markdown")
        install.packages("devtools")
        library(devtools)
        install_github("jepust/SingleCaseES")

4.  After all of these packages are installed, type the following
    commands at the console prompt to start the simulator (the simulator
    should then open in a new window):

        library(SingleCaseES)
        shine_gem_scd()

5.  To exit the simulator, simply close the window in which it appears
    or click the red "Stop" icon in the upper right-hand corner of the
    RStudio console window.
