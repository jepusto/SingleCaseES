#### Parameter definition 

Gingerich (1984) and Busk and Serlin (1992) proposed a within-case standardized
mean difference for use in single-case designs (within-case because it is based
on the data for a single individual, rather than across individuals). Let
$\mu_A$ and $\mu_B$ denote the mean levels of phases A and B, respectively; let
$\sigma_A$ and $\sigma_B$ denote the standard deviations of the outcomes within
phases A and B, respectively. The standardized mean difference parameter
$\delta$ is defined as the difference in means between phase B and phase A,
scaled by the standard deviation of the outcome within phase A:

$$
\delta = \frac{\mu_B - \mu_A}{\sigma_A}.
$$

Note that $\sigma_A$ represents _within-individual_ variability only. In contrast, the SMD applied to a between-groups design involves scaling by a measure of between- and within-individual variability. Thus, the scale of the within-case SMD is _not_ comparable to the scale of the SMD from a between-groups design.

#### Estimation

The SMD $\delta$ can be estimated under the assumption that the observations are mutually independent and have constant variance within each phase. Denote the sample means from phase A and phase B as $\bar{y}_A$ and $\bar{y}_B$, the sample standard deviations from phase A and phase B as $s_A$ and $s_B$, and the number of observations in phase A and phase B as $m$ and $n$, respectively. 

There are two ways that the SMD, depending on whether it is reasonable to assume that the standard deviation of the outcome is constant across phases (i.e., $\sigma_A = \sigma_B$). 

__Baseline SD only.__ Gingerich (1984) and Busk and Serlin (1992) originally suggested scaling by the SD from phase A only, due to the possibility of non-constant variance across phases. Without assuming constant SDs, an estimate of the standardized mean difference is

$$
d_A = \left(1 - \frac{3}{4m - 5}\right) \frac{\bar{y}_B - \bar{y}_A}{s_A}.
$$

The term in parentheses is a small-sample bias correction term (cf. Hedges, 1981; Pustejovsky, 2018). The standard error of this estimate is calculated as

$$
SE_{d_A} = \left(1 - \frac{3}{4m - 5}\right)\sqrt{\frac{1}{m} + \frac{s_B^2}{n s_A^2} + \frac{d_A^2}{2(m - 1)}}.
$$

__Pooled SD.__ If it is reasonable to assume that the SDs are constant across phases, then one can use the pooled sample SD to calculate the SMD, defined as

$$
s_p = \sqrt{\frac{(m - 1)s_A^2 + (n - 1) s_B^2}{m + n - 2}}.
$$

The SMD can then be estimated as 

$$
d_p = \left(1 - \frac{3}{4(m + n) - 9}\right) \frac{\bar{y}_B - \bar{y}_A}{s_p},
$$

with standard error

$$
SE_{d_A} = \left(1 - \frac{3}{4(m + n) - 9}\right)\sqrt{\frac{1}{m} + \frac{1}{n} + \frac{d_p^2}{2(m + n - 2)}}.
$$

__Confidence interval.__ Whether the estimator is based on the baseline or pooled standard deviation, an approximate confidence interval for $\delta$ is given by 

$$
[d - z_{\alpha / 2} \times SE_d,\quad d + z_{\alpha / 2} \times SE_d],
$$

where $z_{\alpha / 2}$ is the $1 - \alpha / 2$ critical value from a standard normal distribution. 

#### Primary references

Busk, P. L., & Serlin, R. C. (1992). Meta-analysis for single-case research. In T. R. Kratochwill & J. R. Levin (Eds.), Single-Case Research Design and Analysis: New Directions for Psychology and Education (pp. 187–212). Hillsdale, NJ: Lawrence Erlbaum Associates, Inc.

Gingerich, W. J. (1984). Meta-analysis of applied time-series data. Journal of Applied Behavioral Science, 20(1), 71–79. doi: [10.1177/002188638402000113](http://dx.doi.org/10.1177/002188638402000113)

#### Additional references

Hedges, L. V. (1981). Distribution theory for Glass’s estimator of effect size and related estimators. Journal of Educational Statistics, 6(2), 107–128. doi: [10.3102/10769986006002107](http://dx.doi.org/10.3102/10769986006002107)

Pustejovsky, J. E. (2018). Procedural sensitivities of effect sizes for single-case designs with behavioral outcome. _Psychological Methods_, forthcoming. https://doi.org/10.1037/met0000179
