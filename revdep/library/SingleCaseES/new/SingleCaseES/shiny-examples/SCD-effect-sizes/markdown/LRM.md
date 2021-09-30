#### Parameter definition 

The log ratio of medians (LRM) effect size quantifies the change in medians from phase A to phase B in proportionate terms, using the natural logarithm of the ratio of medians. This effect size is appropriate for ratio scale outcomes (i.e., where zero indicates the total absence of the outcome) that are skewed or right-censored (Bonett & Price, 2020b). For an outcome where increase is desirable, the LRM parameter is defined as
$$
\lambda = \ln\left(\eta_B / \eta_A\right) = \ln(\eta_B) - \ln(\eta_A),
$$
where $\eta_B$ and $\eta_A$ are the population median for phase B and phase A, respectively, and $\ln()$ is the natural logarithm function. For an outcome where decrease is desirable, the LRM parameter has the opposite sign.

#### Estimation

A natural estimator of the $\lambda$ is given by
$$
LRM = \ln\left(m_B\right) - \ln\left(m_A\right),
$$
where $m_B$ and $m_A$ are the sample medians for phase B and phase A, respectively. Note that the sample median might be zero for either phase B and phase A in some single-case design data, resulting in infinite LRM. 

__Standard error.__ Standard errors and confidence intervals for LRM can be obtained under the assumption that the outcome data within each phase are mutually independent and follow a common distribution. Using the fact that the logarithm of the median is the same or close to the median of the log-transformed outcomes, the standard error for $LRM$ can be calculated using the order statistics within each phase (Bonett & Price, 2020a). Let
$$
l_A = \text{max}\left\{1, \ \frac{m}{2} - \sqrt{m}\right\}, \quad u_A = m - l_A + 1,
$$
and 
$$
l_B = \text{max}\left\{1, \ \frac{n}{2} - \sqrt{n}\right\}, \quad u_B = n - l_B + 1,
$$
and find
$$
q_A = \Phi^{-1}\left(\frac{1}{2^m}\sum_{i=0}^{l_A - 1} \frac{m!}{i!(m - i)!}\right) \quad \text{and} \quad q_B = \Phi^{-1}\left(\frac{1}{2^n}\sum_{j=0}^{l_B - 1} \frac{n!}{j!(n - j)!}\right).
$$
The standard error of LRM is then
$$
SE_{LRM} = \sqrt{\left(\frac{\ln\left(y^B_{(u_B)}\right)-\ln\left(y^B_{(l_B)}\right)}{2\ q_B}\right)^2 + \left(\frac{\ln\left(y^A_{(u_A)}\right)-\ln\left(y^A_{(l_A)}\right)}{2\ q_A}\right)^2},
$$
where $y^A_{(l_A)}, y^A_{(u_A)}$ are the $l_A$ and $u_A$ order statistics of the phase A outcomes and $y^B_{(l_B)}, y^B_{(u_B)}$ are the $l_B$ and $u_B$ order statistics of the phase B outcomes.

__Confidence interval.__ An approximate confidence interval for $\lambda$ is 
$$
\left[LRM - z_{\alpha/2} \times SE_{LRM},\quad LRM + z_{\alpha/2} \times SE_{LRM}\right],
$$
where $z_{\alpha/2}$ is $1 - \alpha/2$ critical value from a standard normal distribution (Bonett & Price, 2020a).

#### Primary reference

Bonett, D. G., & Price, R. M. (2020a). Confidence intervals for ratios of means and medians. _Journal of Educational and Behavioral Statistics, 45_(6), 750-770.

#### Additional reference

Bonett, D. G., & Price, R. M. (2020b). Interval estimation for linear functions of medians in within-subjects and mixed designs. _British Journal of Mathematical and Statistical Psychology, 73_(2), 333-346.