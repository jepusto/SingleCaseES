#### Parameter definition

The log-odds ratio is an effect size index that quantifies the change from phase A to phase B in proportionate change of the prevalence odds. It is appropriate for use with outcomes on a percentage or proportion scale.  The LOR parameter is defined as

$$
\psi = \ln\left(\frac{\mu_B/(1-\mu_B)}{\mu_A/(1-\mu_A)}\right),
$$
where $\mu_A$ and $\mu_B$ denote the mean levels, as measured in proportions, in phases A and B respectively, and $\ln()$ is the natural logarithm function. The use of the odds (e.g. x(1-x)) along with the natural logarithm function is used so that the log odds ratio ranges from $-\infty$ to $\infty$.

####Estimation

Denote the sample means from phase A and phase B as $\bar{y}_A$ and $\bar{y}_B$, the sample standard deviations from phase A and phase B as $s_A$ and $s_B$, and the number of observations in phase A and phase B as $m$ and $n$, respectively. The LOR is estimated as

$$
LOR = \ln\left(\bar{y}_B\right) - \ln\left(1-\bar{y}_B\right) - \frac{s_B^2(2 \bar{y}_B - 1)}{2 n_B (\bar{y}_B)^2(1-\bar{y}_B)^2} - \ln\left(\bar{y}_A\right) + \ln\left(1-\bar{y}_A\right) + \frac{s_A^2(2 \bar{y}_A - 1)}{2 n_A (\bar{y}_A)^2(1-\bar{y}_A)^2}.
$$

This estimator uses a small-sample correction to reduce bias when one or both phases include only a small number of observations.

Under the assumption that the outcomes in each phase are mutually independent, an approximate standard error for $LOR$ is given by

$$
SE_{LOR} = \sqrt{\frac{s^2_A}{n_A(\bar{y}_A)^2(1-\bar{y}_A)^2} + \frac{s^2_B}{n_B(\bar{y}_B)^2(1-\bar{y}_B)^2}}.
$$

Under the same assumption, an approximate confidence interval for $\psi$ is

$$
[LOR - z_\alpha \times SE_{LOR},\quad LOR + z_\alpha \times SE_{LOR}],
$$

#### Primary reference

Pustejovsky, J. E. (2015). Measurement-comparable effect sizes for single-case studies of free-operant behavior. Psychological Methods, 20(3), 342–359. doi: [10.1037/met0000019](http://dx.doi.org/10.1037/met0000019)
