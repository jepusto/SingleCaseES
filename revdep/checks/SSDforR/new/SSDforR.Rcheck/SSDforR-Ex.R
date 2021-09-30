pkgname <- "SSDforR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('SSDforR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ABWilcox")
### * ABWilcox

flush(stderr()); flush(stdout())

### Name: ABWilcox
### Title: Wilcoxon rank-sum test between two phases
### Aliases: ABWilcox

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABWilcox(cry,pcry,"A","B")



cleanEx()
nameEx("ABanova")
### * ABanova

flush(stderr()); flush(stdout())

### Name: ABanova
### Title: Analysis of variance
### Aliases: ABanova

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", 
"B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABanova(cry,pcry)




cleanEx()
nameEx("ABarrow")
### * ABarrow

flush(stderr()); flush(stdout())

### Name: ABarrow
### Title: Draw arrow on graph
### Aliases: ABarrow

### ** Examples
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", 
"B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run ABarrow()




cleanEx()
nameEx("ABautoacf")
### * ABautoacf

flush(stderr()); flush(stdout())

### Name: ABautoacf
### Title: Autocorrelation at any lag for a phase
### Aliases: ABautoacf

### ** Examples
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABautoacf(cry, pcry, "B", 2)



cleanEx()
nameEx("ABautopacf")
### * ABautopacf

flush(stderr()); flush(stdout())

### Name: ABautopacf
### Title: Partial autocorrelation
### Aliases: ABautopacf

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABautopacf (cry, pcry,"A", 3)



cleanEx()
nameEx("ABbinomial")
### * ABbinomial

flush(stderr()); flush(stdout())

### Name: ABbinomial
### Title: Binomial test
### Aliases: ABbinomial

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD1(cry,pcry,"A","week","amount","Crying")
ABbinomial(pcry,"A","B1", 1, 8)



cleanEx()
nameEx("ABdescrip")
### * ABdescrip

flush(stderr()); flush(stdout())

### Name: ABdescrip
### Title: Descriptive Statistics
### Aliases: ABdescrip

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# ABdescrip(cry,pcry)



cleanEx()
nameEx("ABiqr")
### * ABiqr

flush(stderr()); flush(stdout())

### Name: ABiqr
### Title: Interquartile band graph through all phases
### Aliases: ABiqr

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# ABiqr(cry,pcry,"week","amount","Crying")




cleanEx()
nameEx("ABlineD")
### * ABlineD

flush(stderr()); flush(stdout())

### Name: ABlineD
### Title: Add dashed line to a graph
### Aliases: ABlineD

### ** Examples
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run ABlineD(cry)




cleanEx()
nameEx("ABlines")
### * ABlines

flush(stderr()); flush(stdout())

### Name: ABlines
### Title: Draw line
### Aliases: ABlines

### ** Examples
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run ABlines(cry)



cleanEx()
nameEx("ABma")
### * ABma

flush(stderr()); flush(stdout())

### Name: ABma
### Title: Moving average
### Aliases: ABma

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABma(cry, pcry, "A")



cleanEx()
nameEx("ABplot")
### * ABplot

flush(stderr()); flush(stdout())

### Name: ABplot
### Title: Simple line graph
### Aliases: ABplot

### ** Examples
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# ABplot(cry,pcry,"week","amount","Crying")





cleanEx()
nameEx("ABplotm")
### * ABplotm

flush(stderr()); flush(stdout())

### Name: ABplotm
### Title: Multiple line plot
### Aliases: ABplotm

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
yell<-c(3, 4, 2, 5, 5, 4, NA, 1, 2, 2, 2, 0, 0)
pyell<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B")
plotnum(2, 1)
ABplotm(cry,pcry,"week","amount","Crying")
ABplotm(yell,pyell,"week","amount","Yelling")



cleanEx()
nameEx("ABregres")
### * ABregres

flush(stderr()); flush(stdout())

### Name: ABregres
### Title: OLS regression to compare phases
### Aliases: ABregres

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABregres(cry,pcry,"A","B")



cleanEx()
nameEx("ABrf2")
### * ABrf2

flush(stderr()); flush(stdout())

### Name: ABrf2
### Title: Lag-1 autocorrelation (rf2 for small sample size)
### Aliases: ABrf2

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABrf2(cry, pcry, "B1")



cleanEx()
nameEx("ABrobust")
### * ABrobust

flush(stderr()); flush(stdout())

### Name: ABrobust
### Title: Robust regression
### Aliases: ABrobust

### ** Examples


cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABrobust(cry,pcry,"A","B")



cleanEx()
nameEx("ABstat")
### * ABstat

flush(stderr()); flush(stdout())

### Name: ABstat
### Title: Add statistic line(s)
### Aliases: ABstat

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run this ABplot(cry,pcry,"week","amount","Crying")
# run this statement ABstat(cry, pcry, "A", "median")



cleanEx()
nameEx("ABtext")
### * ABtext

flush(stderr()); flush(stdout())

### Name: ABtext
### Title: Add text to graph
### Aliases: ABtext

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# run this ABplot(cry,pcry,"week","amount","Crying")
# now run ABtext("A")



cleanEx()
nameEx("ABtsplot")
### * ABtsplot

flush(stderr()); flush(stdout())

### Name: ABtsplot
### Title: Time series plot for SSD Data
### Aliases: ABtsplot

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABtsplot(cry,pcry,"week","amount","Crying")



cleanEx()
nameEx("ABttest")
### * ABttest

flush(stderr()); flush(stdout())

### Name: ABttest
### Title: T-test comparing phases
### Aliases: ABttest

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABttest(cry,pcry,"A","B")



cleanEx()
nameEx("Append")
### * Append

flush(stderr()); flush(stdout())

### Name: Append
### Title: Append data sets with additional data
### Aliases: Append

### ** Examples

# type Append()



cleanEx()
nameEx("Aregres")
### * Aregres

flush(stderr()); flush(stdout())

### Name: Aregres
### Title: Regression for single phase
### Aliases: Aregres

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Aregres(cry,pcry,"A")



cleanEx()
nameEx("Arimadiff")
### * Arimadiff

flush(stderr()); flush(stdout())

### Name: Arimadiff
### Title: Difference for ARIMA
### Aliases: Arimadiff

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Arimadiff(cry,pcry,"B1",2)



cleanEx()
nameEx("Arimama")
### * Arimama

flush(stderr()); flush(stdout())

### Name: Arimama
### Title: Moving average for ARIMA
### Aliases: Arimama

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Arimama(cry,pcry,"B1",2)



cleanEx()
nameEx("Arobust")
### * Arobust

flush(stderr()); flush(stdout())

### Name: Arobust
### Title: Robust regression for a single phase
### Aliases: Arobust

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Arobust(cry,pcry,"A")



cleanEx()
nameEx("Cchart")
### * Cchart

flush(stderr()); flush(stdout())

### Name: Cchart
### Title: SPC C-chart
### Aliases: Cchart

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Cchart(cry,pcry,"A",2,"week","amount","Crying")



cleanEx()
nameEx("Effectsize")
### * Effectsize

flush(stderr()); flush(stdout())

### Name: Effectsize
### Title: Effect size
### Aliases: Effectsize

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Effectsize(cry,pcry,"A","B")



cleanEx()
nameEx("GABrf2")
### * GABrf2

flush(stderr()); flush(stdout())

### Name: GABrf2
### Title: Autocorrelation for group data
### Aliases: GABrf2

### ** Examples

attend<-c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,NA,
0,1,1,0,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

week<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,NA,6,6,6,6,6,7,7,7,7,7,
8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,12,12,12,12,13,
13,13,13,13,14,14,14,14,14,15,15,15,15,15)

pattend<-c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A",
"A","A","A",NA,"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B"
,"B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B")
# now run: GABrf2(attend,pattend,week,"A")



cleanEx()
nameEx("GABttest")
### * GABttest

flush(stderr()); flush(stdout())

### Name: GABttest
### Title: T-test for group data
### Aliases: GABttest

### ** Examples

attend<-c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,NA,
0,1,1,0,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

week<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,NA,6,6,
6,6,6,7,7,7,7,7,
8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,
12,12,12,12,13,
13,13,13,13,14,14,14,14,14,15,15,15,15,15)

pattend<-c("A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A",
"A","A","A",NA,"B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B"
,"B","B","B","B")
# now run GABttest(attend, pattend, week, "A", "B")



cleanEx()
nameEx("Getcsv")
### * Getcsv

flush(stderr()); flush(stdout())

### Name: Getcsv
### Title: Import .csv file
### Aliases: Getcsv

### ** Examples

# type Getcsv()



cleanEx()
nameEx("Gindex")
### * Gindex

flush(stderr()); flush(stdout())

### Name: Gindex
### Title: G-index
### Aliases: Gindex

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Gindex(cry,pcry,"A","B")



cleanEx()
nameEx("Gline")
### * Gline

flush(stderr()); flush(stdout())

### Name: Gline
### Title: Goal Line
### Aliases: Gline

### ** Examples

# type Getcsv()



cleanEx()
nameEx("Gmedian")
### * Gmedian

flush(stderr()); flush(stdout())

### Name: Gmedian
### Title: Median line for group data
### Aliases: Gmedian

### ** Examples

cohesion<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,
82,89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,86,87,88,89,79,73,75,
74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,
48,50,46,55,51,55,49,50,48,51,33)

week<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,
12,12,12,12,12,12)

pcohesion<-c("A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,"B","B","B",
"B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B")
ABdescrip(cohesion,week)
Gmedian(cohesion,pcohesion,"A")



cleanEx()
nameEx("IQRbandgraph")
### * IQRbandgraph

flush(stderr()); flush(stdout())

### Name: IQRbandgraph
### Title: Interquartile band graph for one phase
### Aliases: IQRbandgraph

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
IQRbandgraph(cry,pcry,"A","week","amount","Crying")



cleanEx()
nameEx("IQRlegend")
### * IQRlegend

flush(stderr()); flush(stdout())

### Name: IQRlegend
### Title: IQR legend
### Aliases: IQRlegend

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
IQRbandgraph(cry,pcry,"A","week","amount","Crying")
IQRlegend()



cleanEx()
nameEx("IQRline")
### * IQRline

flush(stderr()); flush(stdout())

### Name: IQRline
### Title: IQR line for ABplot
### Aliases: IQRline

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# type IQRline(cry, pcry, "A")



cleanEx()
nameEx("IRDabove")
### * IRDabove

flush(stderr()); flush(stdout())

### Name: IRDabove
### Title: Improvement Rate Difference (IRD) calculation
### Aliases: IRDabove

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
IRDabove(esteem,pesteem,"A","B1")



cleanEx()
nameEx("IRDbelow")
### * IRDbelow

flush(stderr()); flush(stdout())

### Name: IRDbelow
### Title: Improvement Rate Difference (IRD) calculation
### Aliases: IRDbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
#IQRbandgraph(cry,pcry,"A","week","amount","Crying")
IRDbelow(cry,pcry,"A","B")



cleanEx()
nameEx("NAPabove")
### * NAPabove

flush(stderr()); flush(stdout())

### Name: NAPabove
### Title: Non-Overlap of All Pairs (NAP) calculation
### Aliases: NAPabove

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
NAPabove(esteem,pesteem,"A","B1")



cleanEx()
nameEx("NAPbelow")
### * NAPbelow

flush(stderr()); flush(stdout())

### Name: NAPbelow
### Title: Non-Overlap of All Pairs (NAP) calculation
### Aliases: NAPbelow

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
NAPbelow(esteem,pesteem,"A","B1")



cleanEx()
nameEx("PANDlegend")
### * PANDlegend

flush(stderr()); flush(stdout())

### Name: PANDlegend
### Title: PAND legend
### Aliases: PANDlegend

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PANDbelow(cry,pcry,"A","B1")
PNDlegend()



cleanEx()
nameEx("PEMabove")
### * PEMabove

flush(stderr()); flush(stdout())

### Name: PEMabove
### Title: PEM - desired values above the reference line
### Aliases: PEMabove

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PEMabove(esteem,pesteem,"A","B1")



cleanEx()
nameEx("PEMbelow")
### * PEMbelow

flush(stderr()); flush(stdout())

### Name: PEMbelow
### Title: PEM - desired values below the reference line
### Aliases: PEMbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PEMbelow(cry,pcry,"A","B")



cleanEx()
nameEx("PNDbelow")
### * PNDbelow

flush(stderr()); flush(stdout())

### Name: PNDbelow
### Title: PND - desired values below the reference line
### Aliases: PNDbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PNDbelow(cry,pcry,"A","B1")



cleanEx()
nameEx("PNDlegend")
### * PNDlegend

flush(stderr()); flush(stdout())

### Name: PNDlegend
### Title: PND legend
### Aliases: PNDlegend

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
#run first
PNDbelow(cry,pcry,"A","B1") #run after complete steps above
PNDlegend()



cleanEx()
nameEx("Pchart")
### * Pchart

flush(stderr()); flush(stdout())

### Name: Pchart
### Title: SPC P-chart
### Aliases: Pchart

### ** Examples

attend<-c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,NA,
0,1,1,0,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

day<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,NA,6,6,
6,6,6,7,7,7,7,7,
8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,
11,12,12,12,12,12,13,
13,13,13,13,14,14,14,14,14,15,15,15,15,15)
Pchart(attend, day, 2, "week", "amount", "Group attendance")



cleanEx()
nameEx("RMGline")
### * RMGline

flush(stderr()); flush(stdout())

### Name: RMGline
### Title: Goal Line for Rmarkdown
### Aliases: RMGline

### ** Examples

# type Getcsv()



cleanEx()
nameEx("RMarrow")
### * RMarrow

flush(stderr()); flush(stdout())

### Name: RMarrow
### Title: Draw arrow on graph. For use with Rmarkdown.
### Aliases: RMarrow

### ** Examples
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", 
"B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run RMarrow(9,10,11,10)




cleanEx()
nameEx("RMlines")
### * RMlines

flush(stderr()); flush(stdout())

### Name: RMlines
### Title: Draws line. For use with Rmarkdown
### Aliases: RMlines

### ** Examples
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run RMlines(cry,13.5)



cleanEx()
nameEx("RMstat")
### * RMstat

flush(stderr()); flush(stdout())

### Name: RMstat
### Title: Add statistic line(s) for RMarkdown
### Aliases: RMstat

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run this ABplot(cry,pcry,"week","amount","Crying")
# run this statement RMstat(cry, pcry, "A", "median",1)



cleanEx()
nameEx("RMtext")
### * RMtext

flush(stderr()); flush(stdout())

### Name: RMtext
### Title: Add text to graph. For use with Rmarkdown
### Aliases: RMtext

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# run this ABplot(cry,pcry,"week","amount","Crying")
# now run RMtext("A",10)



cleanEx()
nameEx("Rchart")
### * Rchart

flush(stderr()); flush(stdout())

### Name: Rchart
### Title: SPC R-chart using mean range
### Aliases: Rchart

### ** Examples

admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,
82,89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,
NA,86,87,88,89,79,73,75,74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,
49,48,50,46,55,51,55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,
11,12,12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",
NA,"B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B")
Rchart(admit, day, 2, "week", "amount", "Admits to Hospital")



cleanEx()
nameEx("Rchartsd")
### * Rchartsd

flush(stderr()); flush(stdout())

### Name: Rchartsd
### Title: SPC R-chart using standard deviation
### Aliases: Rchartsd

### ** Examples

admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,82,89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,86,87,88,89,79,73,75,74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,48,50,46,55,51,55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,
12,12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,"B",
"B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B")
Rchartsd(admit, day, 2, "week", "amount", "Admits to Hospital")



cleanEx()
nameEx("SD1")
### * SD1

flush(stderr()); flush(stdout())

### Name: SD1
### Title: 1-standard deviation band graph
### Aliases: SD1

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD1(cry,pcry,"A","week","amount","Crying")



cleanEx()
nameEx("SD1legend")
### * SD1legend

flush(stderr()); flush(stdout())

### Name: SD1legend
### Title: SD1 legend
### Aliases: SD1legend

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 
2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", 
NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD1(cry,pcry,"A","week","amount","Crying")
SD1legend()



cleanEx()
nameEx("SD2")
### * SD2

flush(stderr()); flush(stdout())

### Name: SD2
### Title: 2-standard deviation band graph
### Aliases: SD2

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA,
"B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD2(cry,pcry,"A","week","amount","Crying")



cleanEx()
nameEx("SD2legend")
### * SD2legend

flush(stderr()); flush(stdout())

### Name: SD2legend
### Title: SD2 legend
### Aliases: SD2legend

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD2(cry,pcry,"A","week","amount","Crying")
SD2legend()



cleanEx()
nameEx("SDAband")
### * SDAband

flush(stderr()); flush(stdout())

### Name: SDAband
### Title: Adds standard deviation bands to an ABplot
### Aliases: SDAband

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run SDAband(cry,pcry,"A",2)



cleanEx()
nameEx("SN")
### * SN

flush(stderr()); flush(stdout())

### Name: SN
### Title: Scientific notation
### Aliases: SN

### ** Examples

SN(2.73e-16)  



cleanEx()
nameEx("SPClegend")
### * SPClegend

flush(stderr()); flush(stdout())

### Name: SPClegend
### Title: SPC legend
### Aliases: SPClegend

### ** Examples

admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,82,89,84,89,
91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,
86,87,88,89,79,73,75,74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,
48,50,46,55,51,55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,
12,12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,
"B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B")
Rchartsd(admit, day, 2, "week", "amount", "Admits to Hospital")
SPClegend()



cleanEx()
nameEx("SPCline")
### * SPCline

flush(stderr()); flush(stdout())

### Name: SPCline
### Title: Draw line on Rchartsd Rchart
### Aliases: SPCline

### ** Examples
admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,82,
89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,86,87,88,89,
79,73,75,74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,48,50,46,55,51,55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,
12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,"B","B",
"B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B")
Rchart(admit, day, 2, "week", "amount", "Admits to Hospital")
# now run SPCline()



cleanEx()
nameEx("SSDforR")
### * SSDforR

flush(stderr()); flush(stdout())

### Name: SSDforR
### Title: List of all functions in SSD for R
### Aliases: SSDforR

### ** Examples
SSDforR()



cleanEx()
nameEx("Savecsv")
### * Savecsv

flush(stderr()); flush(stdout())

### Name: Savecsv
### Title: Save data file
### Aliases: Savecsv

### ** Examples

# type Savecsv()



cleanEx()
nameEx("Trimline")
### * Trimline

flush(stderr()); flush(stdout())

### Name: Trimline
### Title: Trimmed mean line added to ABplot
### Aliases: Trimline

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run Trimline(cry,pcry,"A")



cleanEx()
nameEx("XRchart")
### * XRchart

flush(stderr()); flush(stdout())

### Name: XRchart
### Title: SPC XR-Chart
### Aliases: XRchart

### ** Examples

admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,
82,89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,86,87,88,89,79,73,75,
74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,48,50,46,55,51,
55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,
12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,"B","B",
"B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B")
XRchart(admit, day, 2, "week", "amount", "Admits to Hospital")



cleanEx()
nameEx("Xmrchart")
### * Xmrchart

flush(stderr()); flush(stdout())

### Name: Xmrchart
### Title: SPC XMR-chart
### Aliases: Xmrchart

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Xmrchart(cry, pcry, "A", 2, "week", "amount", "X-mR-Chart")



cleanEx()
nameEx("cdcabove")
### * cdcabove

flush(stderr()); flush(stdout())

### Name: CDCabove
### Title: Conservative Dual Criteria (CDC) desired zone above lines
### Aliases: CDCabove

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
CDCabove(cry,pcry,"A","B")



cleanEx()
nameEx("cdcbelow")
### * cdcbelow

flush(stderr()); flush(stdout())

### Name: CDCbelow
### Title: Conservative Dual Criteria (CDC) desired zone below lines
### Aliases: CDCbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
CDCbelow(cry,pcry,"A","B")



cleanEx()
nameEx("diffchart")
### * diffchart

flush(stderr()); flush(stdout())

### Name: diffchart
### Title: Difference transformation
### Aliases: diffchart

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
diffchart(cry,pcry,"A")



cleanEx()
nameEx("insert")
### * insert

flush(stderr()); flush(stdout())

### Name: insert
### Title: insert
### Aliases: insert

### ** Examples

# This function is not in use by SSD for R end-users.



cleanEx()
nameEx("listnames")
### * listnames

flush(stderr()); flush(stdout())

### Name: listnames
### Title: List variable names
### Aliases: listnames

### ** Examples

# type:  listnames()



cleanEx()
nameEx("meanES")
### * meanES

flush(stderr()); flush(stdout())

### Name: meanES
### Title: Mean Effect Size
### Aliases: meanES

### ** Examples
 #need to open a file




cleanEx()
nameEx("meanNAP")
### * meanNAP

flush(stderr()); flush(stdout())

### Name: meanNAP
### Title: Mean Effect Size
### Aliases: meanNAP

### ** Examples
 #need to open a file




cleanEx()
nameEx("meanabove")
### * meanabove

flush(stderr()); flush(stdout())

### Name: meanabove
### Title: Chi-square - desired values above the mean
### Aliases: meanabove

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 
2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, 
"B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
meanabove(esteem, pesteem, "A","B1")



cleanEx()
nameEx("meanbelow")
### * meanbelow

flush(stderr()); flush(stdout())

### Name: meanbelow
### Title: Chi-square - desired values below the mean
### Aliases: meanbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2,
NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A",
NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
meanbelow(cry,pcry,"A","B1")



cleanEx()
nameEx("medabove")
### * medabove

flush(stderr()); flush(stdout())

### Name: medabove
### Title: Chi-square - desired values above the median
### Aliases: medabove

### ** Examples


esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2,
1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A",
NA, "B", "B", "B", "B", "B", 
"B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
medabove(esteem, pesteem,"A","B1")



cleanEx()
nameEx("medbelow")
### * medbelow

flush(stderr()); flush(stdout())

### Name: medbelow
### Title: Chi-square - desired values below the median
### Aliases: medbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
medbelow(cry,pcry,"A","B1")



cleanEx()
nameEx("metareg")
### * metareg

flush(stderr()); flush(stdout())

### Name: metareg
### Title: Meta Regression
### Aliases: metareg

### ** Examples

ES<-c(.3, .4, .2, .5, .3, .4)
V<-c(.01, .03, .04, .02, .03, .02) 

metareg(ES,V)





cleanEx()
nameEx("metaregi")
### * metaregi

flush(stderr()); flush(stdout())

### Name: metaregi
### Title: Meta Regression with Moderator
### Aliases: metaregi

### ** Examples

ES<-c(.3, .4, .2, .5, .3, .4)
V<-c(.01, .03, .04, .02, .03, .02) 
I<-c(1,3,5,4,6,7)
metaregi(ES,I,V)




cleanEx()
nameEx("pandabove")
### * pandabove

flush(stderr()); flush(stdout())

### Name: PANDabove
### Title: PAND - desired values above the reference line
### Aliases: PANDabove

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PANDabove(esteem,pesteem,"A","B1")



cleanEx()
nameEx("pandbelow")
### * pandbelow

flush(stderr()); flush(stdout())

### Name: PANDbelow
### Title: PAND - desired values below the reference line
### Aliases: PANDbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PANDbelow(cry,pcry,"A","B1")    



cleanEx()
nameEx("pemlegend")
### * pemlegend

flush(stderr()); flush(stdout())

### Name: PEMlegend
### Title: PEM legend
### Aliases: PEMlegend

### ** Examples


cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
#run first
PEMbelow(cry,pcry,"A","B1") 
#run after complete steps above
PEMlegend()




cleanEx()
nameEx("plotnum")
### * plotnum

flush(stderr()); flush(stdout())

### Name: plotnum
### Title: Set graphic environment
### Aliases: plotnum

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
yell<-c(3, 4, 2, 5, 5, 4, NA, 1, 2, 2, 2, 0, 0)
pyell<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B")
plotnum(2, 1)
ABplotm(cry,pcry,"week","amount","Crying")
ABplotm(yell,pyell,"week","amount","Yelling")



cleanEx()
nameEx("pndabove")
### * pndabove

flush(stderr()); flush(stdout())

### Name: PNDabove
### Title: PND - desired values above the reference line
### Aliases: PNDabove

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PNDabove(esteem, pesteem,"A","B1")



cleanEx()
nameEx("regabove")
### * regabove

flush(stderr()); flush(stdout())

### Name: regabove
### Title: Chi-square - desired values above regression line
### Aliases: regabove

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
regabove(cry,pcry,"A","B1")



cleanEx()
nameEx("regbelow")
### * regbelow

flush(stderr()); flush(stdout())

### Name: regbelow
### Title: Chi-square - desired values below regression line
### Aliases: regbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
regbelow(cry,pcry,"A","B1")



cleanEx()
nameEx("robregabove")
### * robregabove

flush(stderr()); flush(stdout())

### Name: robregabove
### Title: Chi-square - desired values above robust regression line
### Aliases: robregabove

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
robregabove(cry,pcry,"A","B1")



cleanEx()
nameEx("robregbelow")
### * robregbelow

flush(stderr()); flush(stdout())

### Name: robregbelow
### Title: Chi-square - desired values below robust regression line
### Aliases: robregbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
robregbelow(cry,pcry,"A","B1")



cleanEx()
nameEx("robustCDCabove")
### * robustCDCabove

flush(stderr()); flush(stdout())

### Name: RobustCDCabove
### Title: Robust Conservative Dual Criteria (CDC) using robust regression
###   desired zone above lines
### Aliases: RobustCDCabove

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
RobustCDCabove(cry,pcry,"A","B")



cleanEx()
nameEx("robustCDCbelow")
### * robustCDCbelow

flush(stderr()); flush(stdout())

### Name: RobustCDCbelow
### Title: Robust Conservative Dual Criteria (CDC) using robust regression
###   desired zone below lines
### Aliases: RobustCDCbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
RobustCDCbelow(cry,pcry,"A","B")



cleanEx()
nameEx("sd1bandgraph")
### * sd1bandgraph

flush(stderr()); flush(stdout())

### Name: sd1bandgraph
### Title: 1-standard deviation band graph for one phase
### Aliases: sd1bandgraph

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
sd1bandgraph(cry,pcry,"A","week","amount","Crying")



cleanEx()
nameEx("sd2bandgraph")
### * sd2bandgraph

flush(stderr()); flush(stdout())

### Name: sd2bandgraph
### Title: 2-standard deviation band graph for one phase
### Aliases: sd2bandgraph

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 
2, 2, 3, 2, 1, 2, NA,
2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, 
"B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
sd2bandgraph(cry,pcry,"A","week","amount","Crying")



cleanEx()
nameEx("trimabove")
### * trimabove

flush(stderr()); flush(stdout())

### Name: trimabove
### Title: Chi-square - desired values above the trimmed mean
### Aliases: trimabove

### ** Examples

esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
trimabove(esteem, pesteem,"A","B1")



cleanEx()
nameEx("trimbelow")
### * trimbelow

flush(stderr()); flush(stdout())

### Name: trimbelow
### Title: Chi-square - desired values below the trimmed mean
### Aliases: trimbelow

### ** Examples

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
trimbelow(cry,pcry,"A","B")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
