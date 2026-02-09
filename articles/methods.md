# Methods

``` r
library(historicalborrow)
```

This vignette defines the models and historical borrowing metrics
supported in the `historicalborrow` package.

## Models

### Common notation

- $y$: vector of patient-specific clinical responses to a continuous
  outcome variable. Ideally, the outcome variable should be some form of
  change from baseline, not the response itself. If the outcome is the
  raw response, then the treatment effect will not be meaningful.
- $y_{ij}$: the element of $y$ corresponding to study $i$ patient $j$.
- $(X)_{ij}$: the row of matrix $X$ corresponding to study $i$ patient
  $j$.
- $\alpha$: Vector of control group mean parameters, one for each study.
  The first elements are for the historical studies, and the last one is
  for the current study.
- $\delta$: Vector of study-specific treatment mean parameters. There is
  one for each combination of study and non-control treatment group.
- $d$: integer index for the elements of $\delta$.
- $b$: integer index for the elements of $\beta$.
- $\beta$: Vector of study-specific baseline covariate parameters.
- $X_{\alpha}$: matrix for the control group mean parameters $\alpha$.
  It has indicator columns to select the appropriate element of $\alpha$
  for each element of $y$.
- $X_{\delta}$: matrix for the treatment mean parameters $\delta$. It
  has indicator columns to select the appropriate element of $\delta$
  for each element of $y$.
- $X_{\beta}$: matrix for the baseline covariate fixed effect parameters
  $\beta$. It has indicator columns to select the appropriate element of
  $\beta$ for each element of $y$.
- $\sigma$: Vector of study-specific residual standard deviations.
- $I( \cdot )$: indicator function.

### Model matrices

Each primary model is parameterized thus:

$$\begin{array}{r}
{E(y) = X_{\alpha}\alpha + X_{\delta}\delta + X_{\beta}\beta}
\end{array}$$

Above, $X_{\alpha}$, $X_{\delta}$, and $X_{\beta}$ are fixed matrices.
$X_{\beta}$ is a conventional model matrix for the baseline covariates
$\beta$, and the details are explained in the “Baseline covariates”
section below. $X_{\alpha}$ is a matrix of zeroes and ones. It is
constructed such that each scalar component of $\alpha$ is the mean
response of the control group in a particular study. Likewise,
$X_{\delta}$ is a matrix of zeroes and ones such that each scalar
component of $\delta$ is the mean response of a non-control treatment
group in a particular study.

To illustrate, let $y_{ijk}$ be the response of patient $k$ in study $i$
in treatment group $j$ (where $j = 1$ is the control group), and let
$\left( X_{\beta}\beta \right)_{ijk}$ be the corresponding scalar
element of the vector $X_{\beta}\beta$. Then,

$$\begin{array}{r}
{E\left( y_{ijk} \right) = I(j = 1)\alpha_{i} + I(j > 1)\delta_{ij} + \left( X_{\beta}\beta \right)_{ijk}}
\end{array}$$

This parameterization is represented in the more compact expression
$X_{\alpha}\alpha + X_{\delta}\delta + X_{\beta}\beta$ in the model
definitions in this vignette.

### Baseline covariates

The baseline covariates model matrix $X_{\beta}$ adjusts for baseline
covariates. It may contain a continuous column for baseline and binary
indicator columns for the levels of user-defined covariates. All these
columns are included if possible, but the method automatically drops
baseline covariate columns to ensure that the combined model matrix
$X_{i}^{*} = \left\lbrack {X_{\alpha}}^{*}\quad{X_{\delta}}^{*}\quad{X_{\beta}}^{*} \right\rbrack_{i}$
is full rank. (Here, $X_{i}^{*}$ denotes the rows of matrix $X$
corresponding to study $i$, with additional rows dropped if the
corresponding elements of $y$ are missing. The additional row-dropping
based on the missingness of $y$ ensures identifiability even when the
user supplies complicated many-leveled factors as covariates.) The
choice of columns to drop from ${X_{\beta}}_{i}$ is determined by the
rank and pivoting strategy of the QR decomposition of $X_{i}$ using the
Householder algorithm with pivoting
([`base::qr()`](https://rdrr.io/r/base/qr.html), LINPACK routine DQRDC).

Separately within each study, each column of $X_{\beta}$ is centered to
have mean 0, and if possible, scaled to have variance 1. Scaling ensures
that the priors on parameters $\beta$ remain relatively diffuse relative
to the input data. Study-level centering ensures that the $\alpha$
parameters truly act as *unconditional* study-specific control group
means (as opposed to conditional on the subset of patients at the
reference level of $X_{\beta}$), and it ensures that borrowing across
$\alpha$ components fully presents as control group borrowing.

### Post-processing

The
[`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md)
function post-processes the results from the model. It accepts MCMC
samples of parameters and returns estimated marginal means of the
response and treatment effect. To estimate marginal means of the
response,
[`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md)
takes group-level averages of posterior samples of fitted values while
dropping covariate adjustment terms from the model
(i.e. $X_{\alpha}\alpha + X_{\delta}\delta$). Because the columns of
$X_{\beta}$ are centered at their means, this choice is mathematically
equivalent to `emmeans::emmeans()` with the `weights = "proportional"`
(Lenth (2016)).

### Mixture model

Functions:

- [`hb_sim_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_mixture.md)
- [`hb_mcmc_mixture()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture.md)

The mixture model analyzes only the data from the current study, so we
use $X_{\alpha}^{\text{mixture}}$ instead of $X_{\alpha}$.
$X_{\alpha}^{\text{mixture}}$ is a one-column matrix to indicate which
elements of $y$ are part of the control group of the current study.

The historical studies contribute to the model through hyperparameters
$\left( m_{\omega} \right)_{i}$ and $\left( s_{\omega} \right)_{i}$. If
study $i$ is a historical study, $\left( m_{\omega} \right)_{i}$ and
$\left( s_{\omega} \right)_{i}$ are the posterior mean and posterior
standard deviation, respectively, of the mean control group response
estimated from the simple model described later. If study $i$ is the
current study, $\left( m_{\omega} \right)_{i}$ and
$\left( s_{\omega} \right)_{i}$ are chosen so the mixture component
Normal($\left( m_{\omega} \right)_{i}$, $\left( s_{\omega} \right)_{i}$)
of study $i$ is diffuse and non-informative. Variable $\omega_{i}$ of
study $i$ is the latent variable of mixture component $i$, and the index
variable $\pi$ chooses which $\omega_{i}$ to use for the current study
control group mean $\alpha$. Hyperparameter $p_{\omega}$ is a constant
vector of prior mixture proportions of each study. The posterior
histogram of $\pi$ gives the posterior mixture proportions.

$$\begin{aligned}
 & {y_{ij}\overset{\text{ind}}{\sim}\text{N}\left( \left( X_{\alpha}^{\text{mixture}}\alpha + X_{\delta}\delta + X_{\beta}\beta \right)_{ij},\ \sigma^{2} \right)} \\
 & {\qquad\alpha = \omega_{\pi}} \\
 & {\qquad\qquad\omega_{i}\overset{\text{ind}}{\sim}\text{Normal}\left( \left( m_{\omega} \right)_{i},\left( s_{\omega} \right)_{i}^{2} \right)} \\
 & {\qquad\qquad\pi \sim \text{Categorical}\left( p_{\omega} \right)} \\
 & {\qquad\delta_{d}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\delta}^{2} \right)} \\
 & {\qquad\beta_{b} \sim \text{Normal}\left( 0,s_{\beta}^{2} \right)} \\
 & {\qquad\sigma \sim \text{Uniform}\left( 0,s_{\sigma} \right)}
\end{aligned}$$

### Hierarchical model

Functions:

- [`hb_sim_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_hierarchical.md)
- [`hb_mcmc_hierarchical()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_hierarchical.md)
- [`hb_s_tau()`](https://wlandau.github.io/historicalborrow/reference/hb_s_tau.md)

The hierarchical model is equivalent to the meta-analytic combined (MAC)
approach analyzes the data from all studies and shrinks the control
group means $\alpha_{i}$ towards a common normal distribution with mean
$\mu$ and variance $\tau^{2}$.

$$\begin{aligned}
 & {y_{ij} \sim \text{N}\left( \left( X_{\alpha}\alpha + X_{\delta}\delta + X_{\beta}\beta \right)_{ij},\ \sigma_{i}^{2} \right)} \\
 & {\qquad\alpha_{i}\overset{\text{ind}}{\sim}\text{Normal}\left( \mu,\tau^{2} \right)} \\
 & {\qquad\qquad\mu \sim \text{Normal}\left( 0,s_{\mu}^{2} \right)} \\
 & {\qquad\qquad\tau \sim f_{\tau}} \\
 & {\qquad\delta_{d}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\delta}^{2} \right)} \\
 & {\qquad\beta_{b}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\beta}^{2} \right)} \\
 & {\qquad\sigma_{i}\overset{\text{ind}}{\sim}\text{Uniform}\left( 0,s_{\sigma} \right)}
\end{aligned}$$

The prior $f_{\tau}$ on $\tau$ is critically important because:

1.  It controls the prior amount of borrowing, and
2.  The prior has a large influence if there are few historical studies
    in the data.

$f_{\tau}$ can either be a flexible half-Student-t distribution with
$d_{\tau}$ degrees of freedom and scale parameter $s_{\tau}$:

$$f_{\tau} = \text{Student-t}\left( 0,s_{\tau},d_{\tau} \right)^{+}$$ or
a uniform distribution with lower bound 0 and upper bound $s_{\tau}$:

$$f_{\tau} = \text{Uniform}\left( 0,s_{\tau} \right)$$

Following the recommendation of Gelman (2006), please use half-Student-t
if the number of historical studies is small and consider uniform for
large numbers of historical studies.

For the half-Student-t distribution, the role of the $s_{\tau}$
parameter is equivalent to the $\sigma$ parameter from the [Student-t
parameterization in the Stan user
manual](https://mc-stan.org/docs/functions-reference/unbounded_continuous_distributions.html#student-t-distribution).

### Independent model

Functions:

- [`hb_sim_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_independent.md)
- [`hb_mcmc_independent()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_independent.md)

The independent model is the same as the hierarchical model, but with
independent control group parameters $\alpha$. We use it as a
no-borrowing benchmark to quantify the borrowing strength of the
hierarchical model and the mixture model.

$$\begin{aligned}
 & {y_{ij} \sim \text{N}\left( \left( X_{\alpha}\alpha + X_{\delta}\delta + X_{\beta}\beta \right)_{ij},\ \sigma_{i}^{2} \right)} \\
 & {\qquad\alpha_{i}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\alpha}^{2} \right)} \\
 & {\qquad\delta_{d}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\delta}^{2} \right)} \\
 & {\qquad\beta_{b}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\beta}^{2} \right)} \\
 & {\qquad\sigma_{i}\overset{\text{ind}}{\sim}\text{Uniform}\left( 0,s_{\sigma} \right)}
\end{aligned}$$

### Pooled model

Functions:

- [`hb_sim_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_sim_pool.md)
- [`hb_mcmc_pool()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_pool.md)

Like the independent model, the pooled model is a benchmark to quantify
the borrowing strength of the hierarchical model and the mixture model.
But instead of the no-borrowing independent model, the pooled model
represents maximum borrowing. Instead of $X_{\alpha}$, below, we use
$X_{\alpha}^{\text{pool}}$, which has only one column to indicate which
observations belong to any control group. In other words, the $\alpha$
parameters are pooled, and $\alpha$ itself is a scalar.

$$\begin{aligned}
 & {y_{ij} \sim \text{N}\left( \left( X_{\alpha}^{\text{pool}}\alpha + X_{\delta}\delta + X_{\beta}\beta \right)_{ij},\ \sigma_{i}^{2} \right)} \\
 & {\qquad\alpha \sim \text{Normal}\left( 0,s_{\alpha}^{2} \right)} \\
 & {\qquad\delta_{d}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\delta}^{2} \right)} \\
 & {\qquad\beta_{b}\overset{\text{ind}}{\sim}\text{Normal}\left( 0,s_{\beta}^{2} \right)} \\
 & {\qquad\sigma_{i}\overset{\text{ind}}{\sim}\text{Uniform}\left( 0,s_{\sigma} \right)}
\end{aligned}$$

### Simple model

Functions:

- [`hb_mcmc_mixture_hyperparameters()`](https://wlandau.github.io/historicalborrow/reference/hb_mcmc_mixture_hyperparameters.md)

The mixture model hyperparameters $\left( m_{\omega} \right)_{i}$ and
$\left( s_{\omega} \right)_{i}$ of study $i$ are obtained by analyzing
the control group data of study $i$ with the simple model below.
$\left( m_{\omega} \right)_{i}$ and $\left( s_{\omega} \right)_{i}$ are
taken to be the estimated posterior mean and posterior standard
deviation, respectively, of $\mu$ from this model.

$$\begin{aligned}
 & {y \sim \text{Normal}\left( \mu,\sigma^{2} \right)} \\
 & {\qquad\mu \sim \text{Normal}\left( 0,s_{\mu}^{2} \right)} \\
 & {\qquad\sigma \sim \text{Uniform}\left( 0,s_{\sigma} \right)}
\end{aligned}$$

## Borrowing metrics

The package supports the following metrics to quantify borrowing.

### Effective sample size (ESS)

See the
[`hb_ess()`](https://wlandau.github.io/historicalborrow/reference/hb_ess.md)
function for an implementation.

Neuenschwander et al. (2010) posit a prior effective sample size metric
for meta-analytic predictive (MAP) priors. In the original paper, the
underlying hierarchical model only uses historical controls, and the
hypothetical new study is the current study of interest. In
`historicalborrow`, we adapt this metric to a hierarchical model which
also includes both control and treatment data from the current study. We
still define $N$ below to be the number of (non-missing) historical
control patients so we can still interpret ESS on the same scale as in
the paper.

For the pooled model, define $V_{0}$ to be the posterior predictive
variance of the control mean $\alpha^{*}$ of a hypothetical new
unobserved study. According to Neuenschwander et al. (2010), it can be
derived as an average of study-specific variances. In practice, we
estimate $V_{0}$ using the average of MCMC samples of
$\frac{1}{\sum\sigma_{i}^{- 2}}$.

$$V_{0}:=\text{Var}\left( \alpha^{*}|y,\tau = 0 \right) = \frac{1}{\sum\sigma_{i}^{- 2}}$$

For the hierarchical model, we define the analogous posterior predictive
variance $V_{\tau}$ using the prior distribution.

$$V_{\tau}:=\text{Var}\left( \alpha^{*}|y \right) = \int E\left\lbrack (\alpha^{*} - E(\alpha^{*}\left| y))^{2} \right|y \right\rbrack \cdot p\left( \alpha^{*}|\mu,\tau \right) \cdot p\left( \mu,\tau|y \right)d\mu d\tau$$

The above integral implies a straightforward method of estimating
$V_{\tau}$ using MCMC samples:

1.  For each MCMC sample $m = 1,\ldots,M$ from the hierarchical model,
    identify samples $\mu^{(m)}$ and $\tau^{(m)}$ of $\mu$ and $\tau$,
    respectively.
2.  Draw $\left( \alpha^{*} \right)^{m}$ from a Normal($\mu^{(m)}$,
    $\left( \tau^{(m)} \right)^{2}$) distribution.
3.  Estimate $V_{\tau}$ as the variance of the collection
    $\left( \alpha^{*} \right)^{1},\left( \alpha^{*} \right)^{2},\ldots,\left( \alpha^{*} \right)^{M}$
    from (2).

Next, define $N$ as the number of non-missing control patients from the
historical studies only. Given $N$, $V_{0}$, and $V_{\tau}$, define the
effective sample size as:

$$\text{ESS}:=N\frac{V_{0}}{V_{\tau}}$$

$\frac{V_{0}}{V_{\tau}}$ is a weight which quantifies the fraction of
historical information that the hierarchical model leverages for
borrowing. Notably, the weight should be 1 if the hierarchical and
pooled model exhibit the same strength of borrowing. Multiplied by $N$,
the quantity becomes a heuristic for the strength of borrowing of the
hierarchical model, measured in terms of the number of historical
patients.

### Precision ratio (hierarchical model only)

The precision ratio is an experimental ad hoc metric and should be used
with caution. It is implemented in the
[`hb_summary()`](https://wlandau.github.io/historicalborrow/reference/hb_summary.md)
function for the hierarchical model.

The precision ratio compares the prior precision of a control mean
response (an $\alpha$ component, numerator) to the analogous precision
of the full conditional distribution (denominator). The former is
$\frac{1}{\tau^{2}}$, and the latter is
$\frac{1}{\tau^{2}} + \frac{n}{\sigma^{2}}$. Here, $n$ is the number of
non-missing patients in the current study, $\sigma^{2}$ is the residual
variance, and $\tau^{2}$ is the variance of study-specific control means
(components of $\alpha$). The full precision ratio is:

$$\begin{array}{r}
\frac{\frac{1}{\tau^{2}}}{\frac{1}{\tau^{2}} + \frac{n}{\sigma^{2}}}
\end{array}$$

The precision ratio comes from the conditional distribution of
$\alpha_{k}$ in the hierarchical model given the other parameters and
the data. More precisely, in this conditional distribution, the mean is
a weighted average between the prior mean and data mean, and the
precision ratio is the weight on the prior mean. This can be seen in a
simpler case with a Bayesian model with a normal data model, a normal
prior on the mean, and known constant variance. For details, see Chapter
2 of Gelman et al. (2020).

### Variance shift ratio

The variance shift ratio is an experimental ad hoc metric and should be
used with caution. It is implemented in the legacy
[`hb_metrics()`](https://wlandau.github.io/historicalborrow/reference/hb_metrics.md)
function.

Let $V_{m}$ be the estimated posterior variance of $\alpha_{I}$ (current
study control group response mean) estimated by model $m$. The variance
shift ratio is:

$$\begin{array}{r}
\frac{V_{m*} - V_{\text{independent}}}{V_{\text{pool}} - V_{\text{independent}}}
\end{array}$$

where $m*$ is a historical borrowing model like the mixture model or
hierarchical model.

### Mean shift ratio (legacy)

The mean shift ratio is not recommended to measure the strength of
borrowing. Rather, it is an informal ad hoc measure of the lack of
commensurability between the current and historical data sources. It is
implemented in the legacy
[`hb_metrics()`](https://wlandau.github.io/historicalborrow/reference/hb_metrics.md)
function.

To define the mean shift ratio, let $\theta_{m}$ be the posterior mean
control group response estimated by model $m$. The mean shift ratio is:

$$\begin{array}{r}
\frac{\theta_{m*} - \theta_{\text{independent}}}{\theta_{\text{pool}} - \theta_{\text{independent}}}
\end{array}$$

where $m*$ is a historical borrowing model like the mixture model or
hierarchical model.

### Posterior mixture proportions (mixture model only)

The posterior mixture proportion of study $i$ is $P(\pi = i)$, and it is
obtained by averaging posterior samples of $\pi$.

## References

Gelman, A. 2006. “Prior Distributions for Variance Parameters in
Hierarchical Models.” *Bayesian Analysis* 1 (3): 515–43.
<https://doi.org/10.1214/06-BA117A>.

Gelman, A., J. B. Carlin, H. S. Stern, D. B. Dunson, A. Vehtari, and D.
B. Rubin. 2020. *Bayesian Data Analysis*. 3rd ed. CRC Press.

Lenth, Russell V. 2016. “Least-Squares Means: The r Package Lsmeans.”
*Journal of Statistical Software* 69 (1): 1–33.
<https://doi.org/10.18637/jss.v069.i01>.

Neuenschwander, B., G. Capkun-Niggli, M. Branson, and D. J.
Spiegelhalter. 2010. “Summarizing Historical Information on Controls in
Clinical Trials.” *Clinical Trials* 7 (1): 5–18.
<https://doi.org/10.1177/1740774509356002>.
