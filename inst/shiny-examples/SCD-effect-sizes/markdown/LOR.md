#### Parameter definition

The log-odds ratio is an effect size index that quantifies the change from phase A to phase B in terms of proportionate change in the odds that a behavior is occurring. It is appropriate for use with outcomes on a percentage or proportion scale. The LOR parameter is defined as

$$
\psi = \ln\left(\frac{\mu_B/(1-\mu_B)}{\mu_A/(1-\mu_A)}\right),
$$
where $\mu_A$ and $\mu_B$ denote the mean levels, as measured in proportions, in phases A and B respectively, and $\ln()$ is the natural logarithm function. The log odds ratio ranges from $-\infty$ to $\infty$, with a value of zero corresponding to no change in mean levels.

#### Estimation

Denote the sample means from phase A and phase B as $\bar{y}_A$ and $\bar{y}_B$, the sample standard deviations from phase A and phase B as $s_A$ and $s_B$, and the number of observations in phase A and phase B as $m$ and $n$, respectively. To account for the possibility that the sample means may be equal to zero or one, even if the mean levels are strictly between zero and one, the LOR is calculated using _truncated_ sample means, given by 

$$
\tilde{y}_A = \text{max} \left\{ \text{min}\\left[\bar{y}_A, 1 - \frac{1}{2 D m}\right], \frac{1}{2 D m}\right\} \qquad \text{and} \qquad \tilde{y}_B = \text{max} \left\{ \text{min}\left[\bar{y}_B, 1 - \frac{1}{2 D n}\right], \frac{1}{2 D n}\right\},
$$

where $D$ is a constant that depends on the scale and recording procedure used to measure the outcomes (Pustejovsky, 2018).

The LOR is then estimated as

$$
LOR = \ln\left(\tilde{y}_B\right) - \ln\left(1-\tilde{y}_B\right) - \frac{s_B^2(2 \tilde{y}_B - 1)}{2 n_B (\tilde{y}_B)^2(1-\tilde{y}_B)^2} - \ln\left(\tilde{y}_A\right) + \ln\left(1-\tilde{y}_A\right) + \frac{s_A^2(2 \tilde{y}_A - 1)}{2 n_A (\tilde{y}_A)^2(1-\tilde{y}_A)^2}.
$$

This estimator uses a small-sample correction to reduce bias when one or both phases include only a small number of observations.

Under the assumption that the outcomes in each phase are mutually independent, an approximate standard error for $LOR$ is given by

$$
SE_{LOR} = \sqrt{\frac{s^2_A}{n_A \tilde{y}_A^2 (1 - \tilde{y}_A)^2} + \frac{s^2_B}{n_B \tilde{y}_B^2 (1 - \tilde{y}_B)^2}}.
$$

Under the same assumption, an approximate confidence interval for $\psi$ is

$$
[LOR - z_\alpha \times SE_{LOR},\quad LOR + z_\alpha \times SE_{LOR}],
$$

#### Primary reference

Pustejovsky, J. E. (2015). Measurement-comparable effect sizes for single-case studies of free-operant behavior. Psychological Methods, 20(3), 342–359. doi: [10.1037/met0000019](http://dx.doi.org/10.1037/met0000019)

#### Additional references

Pustejovsky, J. E. (2018). Using response ratios for meta-analyzing single-case designs with behavioral outcomes. _Journal of School Psychology, 16_, 99-112. https://doi.org/10.1016/j.jsp.2018.02.003
