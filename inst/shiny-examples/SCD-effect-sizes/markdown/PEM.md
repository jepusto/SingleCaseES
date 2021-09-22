#### Definition 

Ma (2006) proposed the percent exceeding the median, defined as the proportion of observations in phase B that improve upon the median of phase A. Ma (2006) did not specify an effect size parameter corresponding to this index.

#### Estimation

For an outcome where increase is desirable, 

$$
\text{PEM} = \frac{1}{n}\sum_{j=1}^n \left[ I(y^B_j > m_A) + 0.5 \times I(y^B_j = m_A) \right],
$$

where $m_A = \text{median}(y^A_1,...,y^A_m)$. For an outcome where decrease is desirable, 

$$
\text{PEM} = \frac{1}{n}\sum_{j=1}^n \left[ I(y^B_j < y^A_{(1)}) + 0.5 \times I(y^B_j = m_A) \right].
$$

The sampling distribution of PEM has not been described, and so standard errors and confidence intervals are not available.

#### Primary reference

Ma, H.-H. (2006). An alternative method for quantitative synthesis of single-subject researches: Percentage of data points exceeding the median. _Behavior Modification, 30_(5), 598–617. https://dx.doi.org/10.1177/0145445504272974
