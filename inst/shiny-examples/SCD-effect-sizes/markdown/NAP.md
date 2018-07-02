#### Parameter definition 

NAP is an estimate of the probability that a randomly selected observation from the B phase improves upon a randomly selected observation from the A phase. For an outcome where increase is desirable, the effect size parameter is

$$\theta = \text{Pr}(Y^B > Y^A) + 0.5 \times \text{Pr}(Y^B = Y^A).$$

For an outcome where decrease is desirable, the effect size parameter is

$$\theta = \text{Pr}(Y^B < Y^A) + 0.5 \times \text{Pr}(Y^B = Y^A).$$


#### Estimation

Let $y^A_1,...,y^A_m$ denote the observations from phase A. Let $y^B_1,...,y^B_n$ denote the observations from phase B. For an outcome where increase is desirable, calculate 

$$q_{ij} = I(y^B_j > y^A_i) + 0.5 I(y^B_j = y^A_i)$$

(for an outcome where decrease is desirable, one would instead use $q_{ij} = I(y^B_j < y^A_i) + 0.5 I(y^B_j = y^A_i)$). The NAP effect size index is then calculated as

$$
\text{NAP} = \frac{1}{m n} \sum_{i=1}^m \sum_{j=1}^n q_{ij}.
$$

__Standard error.__ The standard error for NAP is calculated based on the exactly unbiased variance estimator described by Sen (1967; see also Mee, 1990), which assumes that the observations are mutually independent and are identically distributed within each phase. Let 

$$
Q_1 = \frac{1}{m n^2} \sum_{i=1}^m \left(\sum_{j=1}^n q_{ij}\right)^2, \qquad
Q_2 = \frac{1}{m^2 n} \sum_{j=1}^n \left(\sum_{i=1}^m q_{ij}\right)^2, \qquad \text{and} \qquad
Q_3 = \frac{1}{m n} \sum_{i=1}^m \sum_{j=1}^n q_{ij}^2.
$$

The SE is then calculated as 

$$
SE_{\text{NAP}} = \sqrt{\frac{\text{NAP} - (m + n - 1)\text{NAP}^2 + n Q_1 + m Q_2 - 2 Q_3}{(m - 1)(n - 1)}}.
$$

__Confidence interval.__ The confidence interval for $\theta$ is calculated based on a method proposed by Newcombe (2006; Method 5), which assumes that the observations are mutually independent and are identically distributed within each phase. Using a confidence level of $100\% \times (1 - \alpha)$, the endpoints of the confidence interval are defined as the values of $\theta$ that satisfy the equality 

$$
(\text{NAP} - \theta)^2 = \frac{z^2_{\alpha / 2} h \theta (1 - \theta)}{mn}\left[\frac{1}{h} + \frac{1 - \theta}{2 - \theta} + \frac{\theta}{1 + \theta}\right],
$$

where $h = (m + n) / 2 - 1$ and $z_{\alpha / 2}$ is $1 - \alpha / 2$ critical value from a standard normal distribution. This equation is a fourth-degree polynomial in $\theta$, solved using a numerical root-finding algorithm. 

#### Primary reference

Parker, R. I., & Vannest, K. J. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. _Behavior Therapy, 40_(4), 357--67. doi: [10.1016/j.beth.2008.10.006](http://dx.doi.org/10.1016/j.beth.2008.10.006)

#### Additional references

Mee, W. (1990). Confidence intervals for probabilities and tolerance regions based on a generalization of the Mann-Whitney statistic. _Journal of the American Statistical Association, 85_(411), 793–800. https://doi.org/10.1080/01621459.1990.10474942

Newcombe, R. G. (2006). Confidence intervals for an effect size measure based
on the Mann-Whitney statistic. Part 2: Asymptotic methods and evaluation. 
_Statistics in Medicine, 25_(4), 559--573. doi: [10.1002/sim.2324](http://dx.doi.org/10.1002/sim.2324)

Sen, P. K. (1967). A note on asymptotically distribution-free confidence bounds for P{X<Y}, based on two independent samples. _The Annals of Mathematical Statistics, 29_(1), 95-102. https://www.jstor.org/stable/25049448
