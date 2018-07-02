#### Parameter definition 

The log-response ratio is an effect size index that quantifies the change from phase A to phase B in proportionate terms. It is appropriate for use with outcomes on a ratio scale (i.e., where zero indicates the total absence of the outcome). The LRR parameter is defined as

$$
\psi = \ln\left(\mu_B / \mu_A\right),
$$
where $\mu_A$ and $\mu_B$ denote the mean levels of phases A and B, respectively, and $\ln()$ is the natural logarithm function. The logarithm is used so that the range of the index is less restricted. 

#### LRR-decreasing and LRR-increasing

There are two variants of the LRR (Pustejovsky, 2018), corresponding to whether therapeutic improvements correspond to negative values of the index (LRR-decreasing or LRRd) or positive values of the index (LRR-increasing or LRRi). For outcomes measured as frequency counts or rates, LRRd and LRRi are identical in magnitude but have opposite sign. However, for outcomes measured as proportions (ranging from 0 to 1) or percentages (ranging from 0% to 100%), LRRd and LRRi will differ in both sign and magnitude because the outcomes are first transformed to be consistent with the selected direction of therapeutic improvement.

#### Estimation

Denote the sample means from phase A and phase B as $\bar{y}_A$ and $\bar{y}_B$ (possibly after transforming to be consistent with the direction of therapeutic improvement), the sample standard deviations from phase A and phase B as $s_A$ and $s_B$, and the number of observations in phase A and phase B as $m$ and $n$, respectively. To account for the possibility that the sample means may be equal to zero, even if the mean levels are strictly greater than zero, the LRR is calculated using _truncated_ sample means, given by 
$$
\tilde{y}_A = \text{max} \left\{ \bar{y}_A, \frac{1}{2 D m}\right\} \qquad \text{and} \qquad \tilde{y}_B = \text{max} \left\{ \bar{y}_B, \frac{1}{2 D n}\right\},
$$
where $D$ is a constant that depends on the scale and recording procedure used to measure the outcomes (Pustejovsky, 2018).

The LRR is then estimated as

$$
R = \ln\left(\tilde{y}_B\right) + \frac{s_B^2}{2 n \tilde{y}_B^2} - \ln\left(\tilde{y}_A\right) - \frac{s_A^2}{2 m \tilde{y}_A^2}.
$$

This estimator uses a small-sample correction to reduce bias when the one or both phases include only a small number of observations. 

Under the assumption that the outcomes in each phase are mutually independent, an approximate standard error for $R$ is given by

$$
SE_R = \sqrt{\frac{s_A^2}{m \tilde{y}_A^2} + \frac{s_B^2}{n \tilde{y}_B^2}}.
$$

Under the same assumption, an approximate confidence interval for $\psi$ is 

$$
[R - z_\alpha \times SE_R,\quad R + z_\alpha \times SE_R],
$$

where $z_{\alpha / 2}$ is $1 - \alpha / 2$ critical value from a standard normal distribution. 

#### Primary reference

Pustejovsky, J. E. (2015). Measurement-comparable effect sizes for single-case studies of free-operant behavior. Psychological Methods, 20(3), 342–359. https://dx.doi.org/10.1037/met0000019

Pustejovsky, J. E. (2018). Using response ratios for meta-analyzing single-case designs with behavioral outcomes. _Journal of School Psychology, 16_, 99-112. https://doi.org/10.1016/j.jsp.2018.02.003
