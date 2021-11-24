#### Parameter definition 

Baseline-corrected Tau was proposed by Tarlow (2017) as an alternative to
Tau-U that uses a different, more defensible approach to adjusting for a linear
time trend in the A phase. The index can be calculated with or without conducting a
pre-test for significance of the A phase time trend. We provide two approaches to 
calculate Tau no matter whether the baseline trend is significant or not. 
The first approach is using Kendall's rank correlation (with adjustment for ties), 
as used in Tarlow (2017). The second one is using Tau (non-overlap) index (without adjustment for ties).

If the pre-test for A phase time trend is used, then slope of the baseline trend is first tested using Kendall's rank correlation. If the baseline slope is significantly different from zero, the outcomes are adjusted for baseline trend using Theil-Sen regression, and the residuals from Theil-Sen regression are used to calculate the Kendall's rank correlation or Tau (non-overlap) index. If the baseline slope is not significantly different from zero, then no baseline trend adjustment is made, and the Tau-BC effect size is calculated using Kendall's rank correlation or Tau (non-overlap) index.

If the pre-test for A phase time trend is not used, then the outcomes are adjusted for baseline trend using Theil-Sen regression, regardless of whether the slope is significantly different from zero. The residuals from Theil-Sen regression are then used to calculate the Kendall's rank correlation or Tau (non-overlap) index.

The formal definition of Tau-BC require positing a model for the time trend in the data series. Let $y^A_1,...,y^A_m$ denote the observations from phase A. Let $y^B_1,...,y^B_n$ denote the observations from phase B. Assume that the outcomes can be expressed in terms of a linear time trend and an error term:
$$
y_i^A = \alpha + \beta (i) + \epsilon_i^A, \quad \text{for} \quad i = 1,...,m
$$
$$
y_j^B = \alpha + \beta (m + j) + \epsilon_j^B \quad \text{for} \quad j = 1,...,n.
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
is significant or if the pre-test for baseline time trend is not used, then the 
slope is estimated by Theil-Sen regression. Specifically, we calculate the slope 
between every pair of observations in the A phase:
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
the full data series and calculating Kendall's rank correlation or Tau (non-overlap) 
on the de-trended observations.
Specifically, set $\hat\epsilon_i^A = y_i^A - \hat\beta (i) - \hat\alpha$ for $i
= 1,...,m$ and $\hat\epsilon_j^B = y_j^B - \hat\beta (m + j) - \hat\alpha$. For
an outcome where increase is desirable, calculate
$$w^\epsilon_{ij} = I\left(\hat\epsilon^B_j > \hat\epsilon^A_i\right) - I\left(\hat\epsilon^B_j < \hat\epsilon^A_i\right)$$
or, for an outcome where decrease is desirable, calculate
$$w^\epsilon_{ij} = I\left(\hat\epsilon^B_j < \hat\epsilon^A_i\right) - I\left(\hat\epsilon^B_j > \hat\epsilon^A_i\right).$$
Tau-BC (non-overlap) is then estimated by
$$
\text{Tau}_{BC} = \frac{1}{m n} \sum_{i=1}^m \sum_{j=1}^n w^\epsilon_{ij}.
$$

If calculated with Kendall's rank correlation, Tau-BC is estimated as the rank correlation between $\left\{\hat\epsilon^A_1, \dots, \hat\epsilon^A_m, \hat\epsilon^B_1, \dots, \hat\epsilon^B_n \right\}$ and a dummy coded variable $\left\{0_1,\dots,0_m, 1_1,\dots,1_n \right\}$, with an adjustment for ties (Kendall, 1970, p. 35). Specifically, 
$$
\text{Tau}_{BC}^* = \frac{1}{D} \sum_{i=1}^m \sum_{j=1}^n w^\epsilon_{ij},
$$
where 
$$
D = \sqrt{m \times n \times \left(\frac{(m+n)(m+n-1)}{2}-U\right)}
$$
and $U$ is the number of ties between all possible pairs of observations (including pairs within phase A, pairs within phase B, and pairs of one phase A and one phase B data point). $U$ can be computed as 
$$
U = \sum_{i=1}^{m - 1} \sum_{j = i+1}^m I\left(\hat\epsilon^A_i = \hat\epsilon^A_j\right) + \sum_{i=1}^{n - 1} \sum_{j = i+1}^n I\left(\hat\epsilon^B_i = \hat\epsilon^B_j\right) + \sum_{i=1}^m \sum_{j=1}^n I\left(\hat\epsilon^A_i = \hat\epsilon^B_j\right).
$$

We prefer and recommend to use the Tau-AB form, which divides by $m \times n$ rather than by $D$, because it leads to a simpler interpretation. Furthermore, using $D$ means that $\text{Tau}_{BC}^*$ may be sensitive to variation in phase lengths. To see this sensitivity, consider a scenario where there are no tied values and so every value $\left\{\hat\epsilon^A_1, \dots, \hat\epsilon^A_m, \hat\epsilon^B_1, \dots, \hat\epsilon^B_n \right\}$ is unique. In this case, $U = 0$ and
$$
D = \sqrt{\frac{1}{2} m n (m + n)(m + n - 1)} = m n \sqrt{1 + \frac{m - 1}{2n} + \frac{n - 1}{2m}}.
$$
Thus, the denominator will always be larger than $m n$, meaning that $\text{Tau}_{BC}^*$ will always be smaller than $\text{Tau}_{BC}$. Further, the largest and smallest possible values of $\text{Tau}_{BC}^*$ will be $\pm m n / D$, or about $1 / \sqrt{2}$ when $m$ and $n$ are close to equal. In contrast, the largest and smallest possible values of $\text{Tau}_{BC}$ are always -1 and 1, respectively.

__Standard error and confidence interval.__ The exact sampling distribution of
$\text{Tau}_{BC}^*$ (Kendall, adjusted for ties) has not been described.
Tarlow (2017) proposed to approximate its sampling variance using
$$
SE_{Kendall} = \sqrt{\frac{2 (1 - \text{Tau}_{BC}^2)}{m + n}},
$$
arguing that this would generally be conservative (in the sense of over-estimating 
the true sampling error). When Tau-BC is calculated using Kendall's rank correlation, 
the SingleCaseES package reports a standard error based on this approximation.

When calculated without adjustment for ties, the SingleCaseES package takes a 
different approach for estimating the standard error for $\text{Tau}_{BC}$ (non-overlap), 
reporting approximate standard errors and confidence intervals for $\text{Tau}_{BC}$ 
based on the methods described above for $\text{Tau}$ (non-overlap, without baseline 
trend correction). An important limitation of this approach is that it does not 
account for the uncertainty introduced by estimating the phase A time trend 
(i.e., the uncertainty in $\hat\beta$).

#### Primary reference

Tarlow, K. R. (2017). An improved rank correlation effect size statistic for single-case designs: Baseline corrected Tau. _Behavior Modification, 41_(4), 427–467. https://doi.org/10.1177/0145445516676750

#### Additional reference

Kendall, M. G. (1970). _Rank correlation methods_ (4th edition). Griffin.