#### Parameter definition 

Baseline-corrected Tau was proposed by @tarlow2017improved as an alternative to
Tau-U that uses a different, more defensible approach to adjusting for a
baseline time trend. The index can be calculated with or without conducting a
pre-test for significance of the baseline phase time trend.

If the trend pre-test is used, then slope of the baseline trend is first tested using Kendall's rank correlation. If the baseline slope is significantly different from zero, the outcomes are adjusted for baseline trend using Theil-Sen regression, and the residuals from Theil-Sen regression are used to calculate the Tau (non-overlap) index. If the baseline slope is not significantly different from zero, then no baseline trend adjustment is made, and the Tau-BC effect size is the same as Tau.

If the trend pre-test is not used, then the outcomes are adjusted for baseline trend using Theil-Sen regression, regardless of whether the slope is significantly different from zero. The residuals from Theil-Sen regression are then used to calculate the Tau (non-overlap) index.

The formal definition of Tau-BC require positing a model for the time trend in the data series. Let $y^A_1,...,y^A_m$ denote the observations from phase A. Let $y^B_1,...,y^B_n$ denote the observations from phase B. Assume that the outcomes can be expressed in terms of a linear time trend and an error term:
$$
\begin{aligned}
y_i^A &= \alpha + \beta (i) + \epsilon_i^A, \quad \text{for} \quad i = 1,...,m \\
y_j^B &= \alpha + \beta (m + j) + \epsilon_j^B \quad \text{for} \quad j = 1,...,n.
\end{aligned}
$$
Within each phase, assume that the error terms are independent and share a common distribution. The Tau-BC parameter can then be defined as the Tau parameter for the distribution of the error terms, or
$$
\tau_{BC} = \text{Pr}(\epsilon^B > \epsilon^A) - \text{Pr}(\epsilon^B < \epsilon^A).
$$
An equivalent definition in terms of the outcome distributions is
$$
\tau_{BC} = \text{Pr}\left[Y_j^B - \beta (m + j - i) > Y_i^A \right] - \text{Pr}\left[Y_j^B - \beta (m + j - i) < Y_i^A\right]
$$
for $i=1,...,m$ and $j = 1,...,n$.

#### Estimation

Estimation of $\tau_{BC}$ entails correcting the data series for the baseline
slope $\beta$. If using the baseline trend pre-test, the null hypothesis of
$H_0: \beta = 0$ is first tested using Kendall's rank correlation. If the test
is not significant, then set $\hat\beta = 0$ and $\hat\alpha = 0$. If the test
is significant or if the baseline trend pre-test is eschewed, then the slope is
estimated by Theil-Sen regression. Specifically, we calculate the slope between
every pair of observations in the A phase:
$$
s_{hi} = \frac{y_i^A - y_h^A}{i - h}
$$
for $i = 1,...,m - 1$ and $h = i+1,...,m$. The overall slope estimate is taken
to be the median over all $m(m - 1) / 2$ slope pairs:
$$
\hat\beta = \text{median}\left\{s_{12},...,s_{(m-1)m}\right\}.
$$
The intercept term is estimated by taking the median observation in the A phase
after correcting for the estimated linear time trend:
$$
\hat\alpha = \text{median}\left\{y_1^A - \hat\beta \times 1, \ y_2^A - \hat\beta \times 2, ..., \ y_m^A - \hat\beta \times m\right\}.
$$
However, the intercept estimate is irrelevant for purposes of estimating Tau-BC
because the Tau estimator is a function of ranks and is invariant to a linear
shift of the data series.

After estimating the phase A time trend, $\tau_{BC}$ is estimated by de-trending
the full data series and calculating Tau on the de-trended observations.
Specifically, set $\hat\epsilon_i^A = y_i^A - \hat\beta (i) - \hat\alpha$ for $i
= 1,...,m$ and $\hat\epsilon_j^B = y_j^B - \hat\beta (m + j) - \hat\alpha$. For
an outcome where increase is desirable, calculate
$$w^\epsilon_{ij} = I\left(\hat\epsilon^B_j > \hat\epsilon^A_i\right) - I\left(\hat\epsilon^B_j < \hat\epsilon^A_i\right)$$
or, for an outcome where decrease is desirable, calculate
$$w^\epsilon_{ij} = I\left(\hat\epsilon^B_j < \hat\epsilon^A_i\right) - I\left(\hat\epsilon^B_j > \hat\epsilon^A_i\right).$$
Tau-BC is then estimated by
$$
\text{Tau}_{BC} = \frac{1}{m n} \sum_{i=1}^m \sum_{j=1}^n w^\epsilon_{ij}.
$$

__Standard error and confidence interval.__ The exact sampling distribution of
$\text{Tau}_{BC}$ has not been described. The web calculator reports approximate
an standard error and confidence interval for $\text{Tau}_{BC}$ based on the
methods available for $\text{Tau}$ (without baseline trend correction). A
limitation of this approach is that it does not account for the uncertainty
introduced by estimating the phase A time trend (i.e., the uncertainty in
$\hat\beta$).

#### Primary reference

Tarlow, K. R. (2017). An improved rank correlation effect size statistic for single-case designs: Baseline corrected Tau. _Behavior Modification, 41_(4), 427–467. https://doi.org/10.1177/0145445516676750
