#### Definition 

For an outcome where increase (decrease) is desirable, Parker, Vannest, and Davis (2011) define PAND as the proportion of observations remaining after removing the fewest possible number of observations from either phase so that the highest remaining point from the baseline phase is less than the lowest remaining point from the treatment phase (lowest remaining point from the baseline phase is larger than the highest remaining point from the treatment phase).

This effect size does not have a stable parameter definition because its magnitude depends on the number of observations in each phase (Pustejovsky, 2018). 

#### Estimation

Let $y^A_{(1)},y^A_{(2)},...,y^A_{(m)}$ denote the values of the baseline phase data, sorted in increasing order, and let $y^B_{(1)},y^B_{(2)},...,y^B_{(n)}$ denote the values of the sorted treatment phase data. For an outcome where increase is desirable, PAND is calculated as

$$
\text{PAND} = \frac{1}{m + n} \max \left\{\left(i + j\right) I\left(y^A_{(i)} < y^B_{(n + 1 - j)}\right)\right\},
$$

where $y^A_{(0)} = - \infty$, $y^B_{(n + 1)} = \infty$, and the maximum is taken over the values $0 \leq i \leq m$ and $0 \leq j \leq n$. For an outcome where decrease is desirable, PAND is calculated as 

$$
\text{PAND} = \frac{1}{m + n} \max \left\{\left(i + j\right) I\left(y^A_{(m + 1 - i)} > y^B_{(j)}\right)\right\},
$$

where $y^A_{(m + 1)} = \infty$, $y^B_{(0)} = -\infty$, and the maximum is taken over the values $0 \leq i \leq m$ and $0 \leq j \leq n$.

The sampling distribution of PAND has not been described, and so standard errors and confidence intervals are not available.

#### Primary reference

Parker, R. I., Vannest, K. J., & Davis, J. L. (2011). Effect size in single-case research: A review of nine nonoverlap techniques. _Behavior Modification, 35_(4), 303--22. https://dx.doi.org/10.1177/0145445511399147

Pustejovsky, J. E. (2018). Procedural sensitivities of effect sizes for single-case designs with behavioral outcome. _Psychological Methods_, forthcoming. https://doi.org/10.1037/met0000179
