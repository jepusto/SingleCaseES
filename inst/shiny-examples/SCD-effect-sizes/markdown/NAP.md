#### Parameter definition 

$\theta = \text{Pr}(Y^B > Y^A) + 0.5 \times \text{Pr}(Y^B = Y^A)$

#### Estimator

Let $y^A_1,...,y^A_m$ denote the observations from phase A. Let $y^B_1,...,y^B_n$ denote the observations from phase B. Let $q_{ij} = I(y^B_j > y^A_i) + 0.5 I(y^B_j = y^A_i)$. The NAP effect size index is calculated as

$$
\text{NAP} = \frac{1}{m n} \sum_{i=1}^m \sum_{j=1}^n q_{ij}.
$$

#### Primary reference

Parker, R. I., & Vannest, K. J. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. _Behavior Therapy, 40_(4), 357–67. doi: [10.1016/j.beth.2008.10.006](http://dx.doi.org/10.1016/j.beth.2008.10.006)
