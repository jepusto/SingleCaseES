#### Parameter definition 

Ferron et al. (2020) and Patrona et al. (2022) proposed a percent of goal obtained (PoGO) effect size metric for use in single-case designs. Let $\alpha$ and $\beta$ denote the mean levels of behavior at phases A and B, respectively; let $\gamma$ denote the goal level of behavior. Percent of goal obtained is an effect size index that quantifies the change in the mean level of behavior relative to the goal. The PoGO parameter $\theta$ is defined as:

$$
\theta = \frac{\beta - \alpha}{\gamma - \alpha} \times 100.
$$

#### Estimation

The estimation of PoGO parameter depends on the assumption about the stability of the observations in phases A and B. Denote the sample means from phase A and phase B as $\hat{\alpha}$ and $\hat{\beta}$, the sample standard deviations from phase A and phase B as $s_A$ and $s_B$, and the number of observations in phase A and phase B as $n_A$ and $n_B$, respectively. Under the assumption that the observations are temporally stable, the PoGO effect size estimate is calculated as

$$
PoGO = \frac{\hat{\beta} - \hat{\alpha}}{\gamma - \hat{\alpha}} \times 100.
$$

Under further assumptions that the observations within each phase are normally distributed and mutually independent, the standard error of the PoGO estimate is calculated as

$$
SE_{PoGO} = \frac{\sqrt{ s_{\beta-\alpha}^2 + \frac{(\hat{\beta}-\hat{\alpha})^2}{(\gamma-\hat{\alpha})^2} s_{\gamma-\alpha}^2}}{\gamma - \hat{\alpha}},
$$

where $s_{\beta-\alpha}^2 = \frac{s_A^2}{n_A} + \frac{s_B^2}{n_B}$ and $s_{\gamma-\alpha}^2 = \frac{s_A^2}{n_A}$.

__Confidence interval.__ An approximate confidence interval for $PoGO$ is given by 

$$
[PoGO - z_{\alpha / 2} \times SE_{PoGO},\quad PoGO + z_{\alpha / 2} \times SE_{PoGO}],
$$

where $z_{\alpha / 2}$ is the $1 - \alpha / 2$ critical value from a standard normal distribution. 

#### Primary references

Ferron, J., Goldstein, H., Olszewski, A., & Rohrer, L. (2020). Indexing effects in single-case experimental designs by estimating the percent of goal obtained. _Evidence-Based Communication Assessment and Intervention, 14_(1-2), 6-27. doi: [10.1080/17489539.2020.1732024](https://doi.org/10.1080/17489539.2020.1732024)

Patrona, E., Ferron, J., Olszewski, A., Kelley, E., & Goldstein, H. (2022). Effects of explicit vocabulary interventions for preschoolers: An exploratory application of the Percent of Goal Obtained (PoGO) effect size metric. _Journal of Speech, Language, and Hearing Research_, forthcoming.
