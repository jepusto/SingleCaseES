#### Definition 

For an outcome where increase is desirable, PND is defined as the proportion of observations in the B phase that exceed the highest observation from the A phase. For an outcome where decrease is desirable, PND is the proportion of observations in the B phase that are less than the lowest observation from the A phase. 

This effect size does not have a stable parameter definition because the magnitude of the maximum (or minimum) value from phase A depends on the number of observations in the phase (Allison & Gorman, 1994; Pustejovsky, 2018). 

#### Estimation

For an outcome where increase is desirable, 

$$
\text{PND} = \frac{1}{n} \sum_{j=1}^n I(y^B_j > y^A_{(m)}),
$$

where $y^A_{(m)}$ is the maximum value of $y^A_1,...,y^A_m$. For an outcome where decrease is desirable, 

$$
\text{PND} = \frac{1}{n} \sum_{j=1}^n I(y^B_j < y^A_{(1)}),
$$

where $y^A_{(1)}$ is the minimum value of $y^A_1,...,y^A_m$. 

The sampling distribution of PND has not been described, and so standard errors and confidence intervals are not available.

#### Primary reference

Allison, D. B., & Gorman, B. S. (1994). "Make things as simple as possible, but no simpler." A rejoinder to Scruggs and Mastropieri. _Behaviour Research and Therapy, 32_(8), 885–890. https://doi.org/10.1016/0005-7967(94)90170-8

Pustejovsky, J. E. (2018). Procedural sensitivities of effect sizes for single-case designs with behavioral outcome. _Psychological Methods_, forthcoming. https://doi.org/10.1037/met0000179

Scruggs, T. E., Mastropieri, M. A., & Casto, G. (1987). The quantitative synthesis of single-subject research: Methodology and validation. _Remedial and Special Education, 8_(2), 24--43. https://dx.doi.org/10.1177/074193258700800206
