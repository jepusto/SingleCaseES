#### Parameter definition 

NAP estimates the probability that a randomly selected observation from the B phase improves upon a randomly selected observation from the A phase. For an outcome where increase is desirable, the effect size parameter is

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

The standard error for NAP is calculated based on a non-parametric method proposed by Hanley and McNeil (1982), which assumes that the observations are mutually independent and are identically distributed within each phase. Let 

$$
Q_1 = \frac{1}{m n^2} \sum_{i=1}^m \left(\sum_{j=1}^n q_{ij}\right)^2 
\qquad \text{and} \qquad 
Q_2 = \frac{1}{m^2 n} \sum_{j=1}^n \left(\sum_{i=1}^m q_{ij}\right)^2. 
$$

The SE is then calculated as 

$$
SE_{\text{NAP}} = \sqrt{\frac{1}{mn}\left(\text{NAP}(1 - \text{NAP}) + (n - 1)(Q_1 - \text{NAP}^2) + (m - 1)(Q_2 - \text{NAP}^2)\right)}.
$$

The confidence interval for $\theta$ is calculated based on a method proposed by Newcombe (2006; Method 5), which assumes that the observations are mutually independent and are identically distributed within each phase. Using a confidence level of $100\% \times (1 - \alpha)$, the endpoints of the confidence interval are defined as the values of $\theta$ that satisfy the equality 

$$
(\text{NAP} - \theta)^2 = \frac{z^2_{\alpha / 2} h \theta (1 - \theta)}{mn}\left[\frac{1}{h} + \frac{1 - \theta}{2 - \theta} + \frac{\theta}{1 + \theta}\right],
$$

where $h = (m + n) / 2 - 1$ and $z_{\alpha / 2}$ is $1 - \alpha / 2$ critical value from a standard normal distribution. This equation is a fourth-degree polynomial in $\theta$, solved using a numerical root-finding algorithm. 

#### Primary reference

Parker, R. I., & Vannest, K. J. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. _Behavior Therapy, 40_(4), 357--67. doi: [10.1016/j.beth.2008.10.006](http://dx.doi.org/10.1016/j.beth.2008.10.006)

#### Additional references

Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. _Radiology, 143_, 29--36. doi: [10.1148/radiology.143.1.7063747](http://dx.doi/org/10.1148/radiology.143.1.7063747)

Newcombe, R. G. (2006). Confidence intervals for an effect size measure based
on the Mann-Whitney statistic. Part 2: Asymptotic methods and evaluation. 
_Statistics in Medicine, 25_(4), 559--573. doi: [10.1002/sim.2324](http://dx.doi.org/10.1002/sim.2324)
