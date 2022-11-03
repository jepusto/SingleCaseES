#### Parameter definition 

Ferron et al. (2020) proposed a percent of goal obtained (PoGO) effect size metric for use in single-case designs. Let $\gamma$ denote the goal level of behavior, which must be specified by the analyst or researcher. Percent of goal obtained quantifies the change in the mean level of behavior relative to the goal. The PoGO parameter $\theta$ is defined as:

$$
\theta = \frac{\mu_B - \mu_A}{\gamma - \mu_A} \times 100\%.
$$

#### Estimation


Approaches for estimation of PoGO depend on one's assumption about the stability of the observations in phases A and B. Denote the sample means from phase A and phase B as $\bar{y}_A$ and $\bar{y}_B$, the sample standard deviations from phase A and phase B as $s_A$ and $s_B$, and the number of observations in phase A and phase B as $n_A$ and $n_B$, respectively. Under the assumption that the observations are temporally stable, the PoGO effect size estimate is calculated as

$$
PoGO = \frac{\bar{y}_B - \bar{y}_A}{\gamma - \bar{y}_A} \times 100\%.
$$

Patrona et al. (2022) proposed a method for calculating a standard error for the PoGO estimator under the assumptions that the observations within each phase are mutually independent. The standard error uses an approximation for the standard error of two independent, normally distributed random variables due to Dunlap and Silver (1986). It is calculated as

$$
SE_{PoGO} = \frac{1}{\gamma - \bar{y}_A} \sqrt{\frac{s_A^2}{n_A} + \frac{s_B^2}{n_B} + \left(\frac{\bar{y}_B - \bar{y}_A}{\gamma - \bar{y}_A}\right)^2 \frac{s_A^2}{n_A}}.
$$

__Confidence interval.__ An approximate confidence interval for $PoGO$ is given by 

$$
[PoGO - z_{\alpha / 2} \times SE_{PoGO},\quad PoGO + z_{\alpha / 2} \times SE_{PoGO}],
$$

where $z_{\alpha / 2}$ is the $1 - \alpha / 2$ critical value from a standard normal distribution. 

#### Primary references

Ferron, J., Goldstein, H., Olszewski, A., & Rohrer, L. (2020). Indexing effects in single-case experimental designs by estimating the percent of goal obtained. _Evidence-Based Communication Assessment and Intervention, 14_(1-2), 6-27. doi: [10.1080/17489539.2020.1732024](https://doi.org/10.1080/17489539.2020.1732024)

Patrona, E., Ferron, J., Olszewski, A., Kelley, E., & Goldstein, H. (2022). Effects of explicit vocabulary interventions for preschoolers: An exploratory application of the Percent of Goal Obtained (PoGO) effect size metric. _Journal of Speech, Language, and Hearing Research_, forthcoming.

#### Additional references

Dunlap, W. P., & Silver, N. C. (1986). Confidence intervals and standard error for ratios of normal variables. _Behavior Research Methods, Instruments, & Computers, 18_, 469-471. doi: [10.3758/BF03201412](https://doi.org/10.3758/BF03201412)
