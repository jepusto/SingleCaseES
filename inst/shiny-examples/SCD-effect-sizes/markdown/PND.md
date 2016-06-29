#### Definition 

For an outcome where increase is desirable, PND is defined as the proportion of observations in the B phase that exceed the highest observation from the A phase. For an outcome where decrease is desirable, PND is the proportion of observations in the B phase that are less than the lowest observation from the A phase. 

This effect size does not have a stable parameter definition because the magnitude of the maximum (or minimum) value from phase A depends on the number of observations in the phase. 

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

#### Primary reference

Scruggs, T. E., Mastropieri, M. A., & Casto, G. (1987). The quantitative synthesis of single-subject research: Methodology and validation. _Remedial and Special Education, 8_(2), 24--43. doi: [10.1177/074193258700800206](http://dx.doi.org/10.1177/074193258700800206)
