# GGD -  Gradational Gaussian Distribution

Here is an R package
which generates a reference class object of the basic Gradational Gaussian Distribution
with a frequency distribution approximation or tracing quantile-probability points.

[日本語 README](README.ja.md)


## What is Gradational Gaussian Distribution?

The Gradational Gaussian Distribution (GGD) is a continuous distribution model
that primarily targets unimodal, non-normal distributions.

The GGD is a kind of mixture distribution model
of the normal distributions (Gaussian distributions),
but it is different from the so-called Gaussian Mixture Distribution,
which is represented by linear combinations of the normal distributions.
It is a distribution model in which the mixing ratio of the normal distributions
gradually changes along the x-axis or y-axis directions.
And it is not a convolution of functions of the normal distribution.

I believe that the GGD can be applied as a model
when there is some data that does not follow a normal distribution,
and the reason why the distribution does not follow a normal distribution
is not due to a discrete parameter that can be clustered,
but due to some continuous parameters.

The GGD was invented by the author of this package
after the [Connected Gaussian Distribution (CGD)](https://github.com/Kimitsuna-Goblin/cgd)[^1]
which is an "almighty" quantile-probability points tracer (but a discontinuous distribution).
But I think the GGD is a distribution model that anyone could have come up with,
so I think if there have been some prior researches about it done by our predecessors.
If you have any pre-2021 information on this distribution model, please let me know.


## Overview of this package

The R package on this site can generate the following kinds of distribution models.

0. normal distribution
1. mean of 2 normal distributions (a kind of Gaussian Mixture Distribution)
2. a distribution in which the mixture ratio of 2 normal distributions changes gradually along the horizontal (x-axis) direction (Horizontal Gradational Distribution)
3. a distribution in which the mixture ratio of 2 or 3 normal distributions changes gradually in the vertical (y-axis) direction (Vertical Gradational Distribution)
4. a distribution in which the mixture ratio of 4 normal distributions changes gradually in both the vertical and horizontal directions (Vertical-Horizontal Gradational Distribution)

The above 0. and 1. are not kinds of the GGD, however,
they can be generated for the purpose of comparing distribution models.

Each of the above rough classifications can be further classified
according to the conditions of the normal distribution of the components as follows.

1. a mixture of normal distributions with different means and equal standard deviations (Mean-Differed Sigma-Equaled)
2. a mixture of normal distributions with equal means and different standard deviations (Mean-Equaled Sigma-Differed)
3. a mixture of normal distributions where both mean and standard deviation are different (Mean-Differed Sigma-Differed)

The 4-3 model has the most degrees of freedom
and can represent the most complex distributions in these.
However, simpler models such as 3-1 or 2-2 may be easier to analyze in many cases.

[^1]: In the author's local development environment,
around June 2022, I had created some probability density functions of this GGD earlier.
However, they were just a group of functions that I created intuitively,
not a theoretically developed distribution model.
And since I was not satisfied with them,
I tried to develop the "Connected Gaussian Distribution" package
as continued to study statistics and the Gaussian integral.
Then, when I eventually worked out, I have returned to the GGD.

Yes, I am not a professional of statistics,
just had been a programmer and was a image clustering student.


## Major functions

| Type      | Name                  | Overview                                                  |
| :-------: | :-------------------: | :-------------------------------------------------------- |
| Generator | nls.freq              | Generates a GGD class object that
                                      approximates a frequency distribution.                    |
| 〃        | nls.freq.all          | Attempts to approximate the frequency distribution
                                      with all supported kinds of distributions.                |
| Generator | trace.q               | Generates a GGD class object that traces
                                      quantile-probability points.                              |
| Field     | \$mean                | The mean of the distribution.                             |
| Method    | \$d                   | Returns the values of the probability density function.   |
| 〃        | \$p                   | Returns the probability for x-coordinates (quantiles).    |
| 〃        | \$q                   | Returns the x-coordinates (quantiles) for probabilities.  |
| 〃        | $r                    | Returns random samples following the distribution.        |
| 〃        | \$sd, \$usd, \$lsd    | Returns the standard deviation, upper standard deviation
                                      and lower standard deviation of the distribution[^2].     |
| 〃        | \$tex                 | Displays the probability density function
                                      and cumulative distribution function in TeX format.       |

## Installation

<pre>
# Install devtools from CRAN
install.packages( "devtools" )

# Then use devtools::install_github( "user/repository" ) to install cgd package from GitHub
devtools::install_github( "Kimitsuna-Goblin/ggd" )
</pre>


## Kinds of distributions

### 0. Normal Distribution

#### Distribution function and cumulative distribution function

The probability density function $f(x)$ and cumulative distribution function $\Phi(x)$ are expressed as follows.

$$
\begin{align}
f(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma^2} \right)\\
\Phi(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma^2} \right) dt
\end{align}
$$

#### Description

This class is the so-called normal distribution.

When approximating a frequency distribution, the result may be a normal distribution.
In that case, do not hesitate to apply the normal distribution as the model.

You can trace 2 quantile-probability points (e.g., tertiles) (can it be called a trace for only 2 points?).

#### Names of "kind" at this package

+ Normal Distribution


### 1. Mean of 2 Normal Distributions

#### Distribution function and cumulative distribution function

The probability density function $g(x)$ and cumulative distribution function $\Psi(x)$ are
expressed as follows using the 2 normal distributed probability density functions $f_1(x)$ and $f_2(x)$
and cumulative distribution functions $\Phi_1(x)$ and $\Phi_2(x)$.

$$
\begin{align}
g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) )\\
\Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) )
\end{align}
$$

#### Description

This class is an example of the Gaussian Mixture Distribution,
supported to see how the GGD differs from it.

The Gaussian Mixture Distribution is essentially a means to split data from a distribution
that is not unimodal or does not follow a normal distribution
into multiple clusters of data that follow a normal distribution.

Use it to approximate the frequency distribution
as a guide to determine whether the Gaussian Mixture Distribution is applicable,
or the GGD should be applied.

Meanwhile, it is allow to try tracing 3 or 4 quantile-probability points
with the cumulative density function.

#### Names of "kind" at this package

+ Mean of Mean-Differed Sigma-Equaled 2 Normal Distributions
+ Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions
+ Mean of Mean-Differed Sigma-Differed 2 Normal Distributions


### 2. Horizontal Gradational Distribution

#### Distribution function and cumulative distribution function

The probability density function $g(x)$ and cumulative distribution function $\Psi(x)$ are
expressed as follows using the 2 normal distributed probability density functions $f_1(x)$ and $f_2(x)$
and cumulative distribution functions $\Phi_1(x)$ and $\Phi_2(x)$.

$$
\begin{align}
g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x)\\
\Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2
\end{align}
$$

#### Description

In this class, as $x$ increases as $-\infty \to \infty$,
the weight of $f_1(x)$ conversely decreases as $1 \to 0$
and the weight of $f_2(x)$ increases as $0 \to 1$.

When tracing quantile-probability points with the cumulative density function,
you can trace 3 or 4 quantile-probability points.
For example, the quartiles for $p = 0.25, 0.5, 0.75$ can be traced
with very little error (as long as they are not overly distorted).

However, if you know the frequency distribution,
it is recommended to approximate the frequency distribution
rather than tracing quantile-probability points.
This is because generally you cannot be sure that the original data can be properly modeled
by just tracing 3 or 4 quartiles.

#### Names of "kind" at this package

+ Mean-Differed Sigma-Equaled Horizontal Gradational Distribution
+ Mean-Equaled Sigma-Differed Horizontal Gradational Distribution
+ Mean-Differed Sigma-Differed Horizontal Gradational Distribution


### 3. Vertical Gradational Distribution

#### Distribution function and cumulative distribution function

The probability density function $g(x)$ and cumulative distribution function $\Psi(x)$
are expressed as follows using 2 or 3 normally distributed probability density functions $f_i(x)$
and cumulative distribution function $\Phi_i(x)$ $(i = 1, 2, 3)$.

In the following equation,
$\Phi^\ast_i(x)$ represents the cumulative distribution function of the normal distribution
with mean $\mu_i$ and standard deviation $\displaystyle \frac{\sigma_i}{\sqrt{2}}$.

3-1. Vertical gradation with 2 normal distributions (1 for head and 1 for tail)

$$
\begin{align}
g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x)\\
\Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x)
\end{align}
$$

3-2. Vertical gradation with 3 normal distributions (1 for head and 2 for tail (for lower side and upper side))

$$
\begin{align}
g(x) &= g_1(x) + g_2(x) + g_3(x) \\
\Psi(x) &= \Psi_1(x) + \Psi_2(x) + \Psi_3(x)\\
\\
g_1(x) &= \left\lbrace
\begin{array}{l}
\left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) & (x \leq \mu_1)\\
0 & (x > \mu_1)\\
\end{array} \right.\\
g_2(x) &= \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x)\\
g_3(x) &= \left\lbrace
\begin{array}{l}
0 & (x < \mu_3)\\
\left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x) & (x \geq \mu_3)
\end{array} \right.\\
\\
\Psi_1(x) &= \min \left( \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x), \ \dfrac{2 - \sqrt{2}}{4} \right)\\
\Psi_2(x) &= \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x)\\
\Psi_3(x) &= \max \left( 0, \ \Phi_3(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_3(x) - \dfrac{2 - \sqrt{2}}{4} \right)
\end{align}
$$

#### Description

In this class, as each value of the normal distributed probability density functions
$f_i(x)$ $(i = 1, 2, 3)$ increase $0 \to f_i(\mu_i)$,
the weights of $f_1(x)$ and $f_3(x)$ conversely decrease $1 \to 0$
and the weight of $f_2(x)$ gradually increases from $0$ to its maximum value
(equal to or smaller than $1$).

The image of the probability density function is such that
the closer x to the tail of the normal distribution, the greater the weights of $f_1(x)$ and $f_3(x)$,
and the closer x to the head of normal the distribution, the greater the weight of $f_2(x)$.

For each standard deviation $\sigma_i$ $(i = 1, 2, 3)$,
it should be $\sigma_2 \leq \sigma_1, \sigma_3$.
In other words, the standard deviation should be larger for the tail side than for the head side.
Conversely, if S.D. for the head side is larger,
the head will not ride on the tail body and the tail body will "break through" the head.

When tracing quantile-probability points with the cumulative density function,
you can trace from 3 to 6 quantile-probability points.

However, this vertical gradational distribution
is not well suited for tracing equally spaced probability points.
This distribution model is better suited for tracing quantiles near the head and near the tale
(i.e. points far and near from the mean).
For example, quantile-probability points with $p = 0.1, 0.4, 0.5, 0.6, 0.9$
can be traced with very little error (as long as they are not overly distorted).

If you want to trace quantile-probability points for symmetrical distributions,
you can use quantile-probability points biased to one side
(e.g. points with $p = 0.1, 0.4, 0.5$).

#### Names of "kind" at this package

3-1. Vertical gradation with 2 normal distributions (1 for head and 1 for tail)
+ Mean-Differed Sigma-Equaled Vertical Gradational Distribution
+ Mean-Equaled Sigma-Differed Vertical Gradational Distribution
+ Mean-Differed Sigma-Differed Vertical Gradational Distribution


3-2. Vertical gradation with 3 normal distributions (1 for head and 2 for tail (for lower side and upper side))
+ 3-Mean-Differed Sigma-Equaled Vertical Gradational Distribution
+ Mean-Equaled 3-Sigma-Differed Vertical Gradational Distribution
+ 3-Mean-Differed 3-Sigma-Differed Vertical Gradational Distribution


### 4. Vertical-Horizontal Gradational Distribution

#### Distribution function and cumulative distribution function

The probability density function $g(x)$ and cumulative distribution function $\Psi(x)$
are expressed as follows using the probability density function $f_{i,j}(x)$
and cumulative distribution function $\Phi_{i,j}(x)$ $(i, j = 1, 2)$ for 4 normal distributions.

In the following equation,
$\Phi^\ast_i(x)$ represents the cumulative distribution function of the normal distribution
with mean $\mu_i$ and standard deviation $\displaystyle \frac{\sigma_i}{\sqrt{2}}$.

$$
\begin{align}
g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x)\\
\Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2\\
\\
g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x)\\
\Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x)
\end{align}
$$

#### Description

In this class, the probability density function $g(x)$ is borne by
4 normally distributed probability density functions
$f_{1,1}(x), f_{1,2}(x), f_{2,1}(x)$ and $f_{2,2}(x)$.

The $f_{1,1}(x)$ is for the lower-tail side of the distribution (where $x$ is less than the mean),
$f_{1,2}(x)$ is for the lower-head side of the distribution,
$f_{2,1}(x)$ is for the upper-head side of the distribution (where $x$ is greater than the mean),
and $f_{2,2}(x)$ is for the upper-tail side of the distribution, respectively.

Of the distribution models in this package,
it has the most degrees of freedom and can represent the most complex distributions.

When tracing quantile-probability points with the cumulative density function,
you can trace from 5 to 8 quantile-probability points.
For example, the quantile points at $p = 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9$
can be traced with little error (as long as they are not overly distorted).

Quantile-probability points larger than 9 cannot be traced with this package.
If you really want to trace more than 9 points without error,
please use the discontinuous distribution of
[Connected Gaussian Distribution (CGD)](https://github.com/Kimitsuna-Goblin/cgd).

#### Names of "kind" at this package

+ Mean-Differed Sigma-Equaled Vertical-Horizontal Gradational Distribution
+ Mean-Equaled Sigma-Differed Vertical-Horizontal Gradational Distribution
+ Mean-Differed Sigma-Differed Vertical-Horizontal Gradational Distribution

