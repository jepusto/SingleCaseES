#### Definition 

The robust improvement rate difference is defined as the robust phi coefficient corresponding to a certain $2 \times 2$ table that is a function of the degree of overlap between the observations each phase (Parker, Vannest, & Davis, 2011). 

This effect size does not have a stable parameter definition because its magnitude depends on the number of observations in each phase (Pustejovsky, 2018). 

#### Estimation

Let $y^A_{(1)},y^A_{(2)},...,y^A_{(m)}$ denote the values of the baseline phase data, sorted in increasing order, and let $y^B_{(1)},y^B_{(2)},...,y^B_{(n)}$ denote the values of the sorted treatment phase data. Let $y^A_{(0)} = y^B_{(0)} = -\infty$ and $y^A_{(m + 1)} = y^B_{(n + 1)} = \infty$. For an outcome where increase is desirable, let $\tilde{i}$ and $\tilde{j}$ denote the values that maximize the quantity

$$
\left(i + j\right) I\left(y^A_{(i)} < y^B_{(n + 1 - j)}\right)
$$
for $0 \leq i \leq m$ and $0 \leq j \leq n$. For an outcome where decrease is desirable, let $\tilde{i}$ and $\tilde{j}$ instead denote the values that maximize the quantity

$$
\left(i + j\right) I\left(y^A_{(m + 1 - i)} > y^B_{(j)}\right).
$$

Now calculate the $2 \times 2$ table

$$
\\begin{array}{|c|c|} \\hline
m - \\tilde{i} & \\tilde{j} \\\\ \\hline
\\tilde{i} & n - \\tilde{j} \\\\ \\hline
\\end{array}
$$

Parker, Vannest, and Brown (2009) proposed the _non-robust_ improvement rate difference, which is equivalent to the phi coefficient from this table. Parker, Vannest, and Davis (2011) proposed to instead use the _robust_ phi coefficient, which involves modifying the table so that the row- and column-margins are equal. Robust IRD is thus equal to 

$$
\text{IRD} = \frac{n - m - \tilde{i} - \tilde{j}}{2 n} - \frac{m + n - \tilde{i} - \tilde{j}}{2 m}.
$$

Robust IRD is algebraically related to PAND as

$$
\text{IRD} = 1 - \frac{(m + n)^2}{2mn}\left(1 - \text{PAND}\right). 
$$


#### Primary reference

Parker, R. I., Vannest, K. J., & Davis, J. L. (2011). Effect size in single-case research: A review of nine nonoverlap techniques. _Behavior Modification, 35_(4), 303--22. https://dx.doi.org/10.1177/0145445511399147

#### Additional references 

Parker, R. I., Vannest, K. J., & Brown, L. (2009). The improvement rate difference for single-case research. _Exceptional Children, 75_(2), 135–150. https://dx.doi.org/10.1177/001440290907500201

Pustejovsky, J. E. (2018). Procedural sensitivities of effect sizes for single-case designs with behavioral outcome. _Psychological Methods_, forthcoming. https://doi.org/10.1037/met0000179
