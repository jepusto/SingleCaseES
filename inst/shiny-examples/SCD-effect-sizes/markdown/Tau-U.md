#### Definition 

Tau-U is one of several effect sizes proposed by Parker, Vannest, Davis, and Sauber (2011). The Tau-U variant is similar to Tau, but includes an adjustment for baseline time trends. For an outcome where increase is desirable, the index is calculated as the Kendall rank-correlation coefficient between the outcome observations and a binary variable indicating phase B, minus $(m - 1) / (2n)$ times the Kendall rank-correlation coefficient between the outcome observations and the session numbers within the baseline phase. 

This effect size does not have a stable parameter definition. 

#### Estimation

Let $y^A_1,...,y^A_m$ denote the observations from phase A. Let $y^B_1,...,y^B_n$ denote the observations from phase B. For an outcome where increase is desirable, calculate 

$$w^{AB}_{ij} = I(y^B_j > y^A_i) - I(y^B_j < y^A_i)$$

and 

$$w^{AA}_{ij} = I(y^A_j > y^A_i) - I(y^A_j < y^A_i)$$

(for an outcome where decrease is desirable, one would instead use $w^{AB}_{ij} = I(y^B_j < y^A_i) - I(y^B_j > y^A_i)$ and $w^{AA}_{ij} = I(y^A_j < y^A_i) - I(y^A_j > y^A_i)$). The effect size index is then calculated as

$$
\text{Tau-U} = \frac{1}{m n} \left(\sum_{i=1}^m \sum_{j=1}^n w^{AB}_{ij} - \sum_{i=1}^{m - 1} \sum_{j=i + 1}^m w^{AA}_{ij}\right). 
$$

This estimator is equivalent to the Spearman rank-correlation between the outcome observations $(y^A_1,...,y^A_m,y^B_1,...,y^B_n)$ and the sequence $x_1,...,x_{m + n}$, where $x_i = m + 1 - i$ for $i = 1,...,m$ and $x_i = m + 1$ for $i = m + 1,..., m + n$. 

#### Primary reference

Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining nonoverlap and trend for single-case research: Tau-U. _Behavior Therapy, 42_(2), 284--299. doi: [10.1016/j.beth.2010.08.006](http://dx.doi.org/10.1016/j.beth.2010.08.006)
