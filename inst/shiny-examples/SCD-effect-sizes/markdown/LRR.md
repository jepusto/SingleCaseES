#### Parameter definition 

The log-response ratio is an effect size index that quantifies the change from phase A to phase B in proportionate terms. It is appropriate for use with outcomes on a ratio scale (i.e., where zero indicates the total absence of the outcome). The LRR parameter is defined as

$$
\psi = \ln\left(\mu_B / \mu_A\right),
$$
where $\mu_A$ and $\mu_B$ denote the mean levels of phases A and B, respectively, and $\ln()$ is the natural logarithm function. The logarithm is used so that the range of the index is less restricted. 

#### Estimation

Denote the sample means from phase A and phase B as $\bar{y}_A$ and $\bar{y}_B$, the sample standard deviations from phase A and phase B as $s_A$ and $s_B$, and the number of observations in phase A and phase B as $m$ and $n$, respectively. The LRR is estimated as

$$
R = \ln\left(\bar{y}_B\right) + \frac{s_B^2}{2 n \bar{y}_B^2} - \ln\left(\bar{y}_A\right) - \frac{s_A^2}{2 m \bar{y}_A^2}.
$$

This estimator uses a small-sample correction to reduce bias when the one or both phases include only a small number of observations. 

Under the assumption that the outcomes in each phase are mutually independent, an approximate standard error for $R$ is given by

$$
SE_R = \sqrt{\frac{s_A^2}{m \bar{y}_A^2} + \frac{s_B^2}{n \bar{y}_B^2}}.
$$

Under the same assumption, an approximate confidence interval for $\psi$ is 

$$
[R - z_\alpha \times SE_R,\quad R + z_\alpha \times SE_R],
$$

where $z_{\alpha / 2}$ is $1 - \alpha / 2$ critical value from a standard normal distribution. 

#### Primary reference

Pustejovsky, J. E. (2015). Measurement-comparable effect sizes for single-case studies of free-operant behavior. Psychological Methods, 20(3), 342–359. doi: [10.1037/met0000019](http://dx.doi.org/10.1037/met0000019)
