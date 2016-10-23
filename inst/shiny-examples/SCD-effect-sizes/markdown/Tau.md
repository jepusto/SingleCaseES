#### Parameter definition 

Tau is one of several effect sizes proposed by Parker, Vannest, Davis, and Sauber (2011) and known collectively as "Tau-U." The basic estimator Tau does not make any adjustments for time trends. For an outcome where increase is desirable, the effect size parameter is

$$\tau = \text{Pr}(Y^B > Y^A) - \text{Pr}(Y^B < Y^A)$$

(for an outcome where decrease is desirable, the effect size parameter would have the opposite sign). This parameter is a simple linear transformation of the NAP parameter $\theta$:

$$\tau = 2 \theta - 1.$$

#### Estimation

Let $y^A_1,...,y^A_m$ denote the observations from phase A. Let $y^B_1,...,y^B_n$ denote the observations from phase B. For an outcome where increase is desirable, calculate 

$$w_{ij} = I(y^B_j > y^A_i) - I(y^B_j < y^A_i)$$

(for an outcome where decrease is desirable, one would instead use $w_{ij} = I(y^B_j < y^A_i) - I(y^B_j > y^A_i)$). The effect size index is then calculated as

$$
\text{Tau} = \frac{1}{m n} \sum_{i=1}^m \sum_{j=1}^n w_{ij} = 2 \times \text{NAP} - 1.
$$

Standard errors and confidence intervals for Tau are calculated using transformations of the corresponding SEs and CIs for NAP. All of the methods assume that the observations are mutually independent and are identically distributed within each phase. The standard error for Tau is calculated as $SE_{\text{Tau}} = 2 SE_{\text{NAP}}$, where $SE_{\text{NAP}}$ is the standard error for NAP from Hanley and McNeil (1982). The CI for $\tau$ is calculated as 

$$
[L_{\tau}, U_{\tau}] = [2 L_{\theta} - 1, 2 U_{\theta} - 1],
$$

where $L_{\theta}$ and $U_{\theta}$ are the lower and upper bounds of the CI for $\theta$, calculated using a method proposed by Newcombe (2006, method 5).

#### Primary reference

Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining nonoverlap and trend for single-case research: Tau-U. _Behavior Therapy, 42_(2), 284--299. doi: [10.1016/j.beth.2010.08.006](http://dx.doi.org/10.1016/j.beth.2010.08.006)

#### Additional references

Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. _Radiology, 143_, 29--36. doi: [10.1148/radiology.143.1.7063747](http://dx.doi/org/10.1148/radiology.143.1.7063747)

Newcombe, R. G. (2006). Confidence intervals for an effect size measure based
on the Mann-Whitney statistic. Part 2: Asymptotic methods and evaluation. 
_Statistics in Medicine, 25_(4), 559--573. doi: [10.1002/sim.2324](http://dx.doi.org/10.1002/sim.2324)
