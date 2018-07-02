#### Definition 

Tau-U is one of several effect sizes proposed by Parker, Vannest, Davis, and Sauber (2011). The Tau-U variant is similar to Tau, but includes an adjustment for baseline time trends. For an outcome where increase is desirable, the index is calculated as Kendall's $S$ statistic for the comparison between the phase B data and the phase A data, plus Kendall's $S$ statistic for the A phase observations, scaled by the product of the number of observations in each phase. 

This effect size does not have a stable parameter definition (Tarlow, 2017). 

#### Estimation

Let $y^A_1,...,y^A_m$ denote the observations from phase A. Let $y^B_1,...,y^B_n$ denote the observations from phase B. For an outcome where increase is desirable, calculate 

$$w^{AB}_{ij} = I(y^B_j > y^A_i) - I(y^B_j < y^A_i)$$

and 

$$w^{AA}_{ij} = I(y^A_j > y^A_i) - I(y^A_j < y^A_i)$$

(for an outcome where decrease is desirable, one would instead use $w^{AB}_{ij} = I(y^B_j < y^A_i) - I(y^B_j > y^A_i)$ and $w^{AA}_{ij} = I(y^A_j < y^A_i) - I(y^A_j > y^A_i)$). The effect size index is then calculated as

$$
\text{Tau-U} = \frac{1}{m n} \left(\sum_{i=1}^m \sum_{j=1}^n w^{AB}_{ij} - \sum_{i=1}^{m - 1} \sum_{j=i + 1}^m w^{AA}_{ij}\right). 
$$

#### Primary reference

Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011). Combining nonoverlap and trend for single-case research: Tau-U. _Behavior Therapy, 42_(2), 284--299. https://dx.doi.org/10.1016/j.beth.2010.08.006

Tarlow, K. R. (2017). An improved rank correlation effect size statistic for single-case designs: Baseline corrected Tau. _Behavior Modification, 41_(4), 427–467. https://doi.org/10.1177/0145445516676750
