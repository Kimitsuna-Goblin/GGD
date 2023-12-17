# GGD -  Gradational Gaussian Distribution

Here is an R package which generates a reference class object provides a Gradational Gaussian Distribution;
it approximates an asymmetric frequency distribution or traces quantiles accurately.

[日本語](README.ja.md)


## What is the Gradational Gaussian Distribution?

The Gradational Gaussian Distribution (GGD) is one of continuous distribution models
for mainly modeling asymmetric unimodal data which do not follow a normal distribution.

The GGD is alike the Gaussian mixture distribution model (GMM) but different.
The GMM is represented by linear combinations of some normal distributions,
and is often used for clustering of mixed data.
On the other hand, the GGD is a distribution of which mixes some normal distributions
with gradually changing ratio along the x-axis or y-axis directions,
and treat non-normal distribution data as it is.
Please remark that the GGD is not a convolution of normal distributions.

I think that the GGD model can be applied as a distribution model of data
which is a bit like a normal distribution but never follows any normal distribution model
because some effects by (hidden) continuous parameters.
The GGD model may not able to exclude disturbing effects from a normal distribuion like GMM,
but may help to find out some existences of effects by hidden continuous parameters in data.

The GGD model was invented by the author of this package,
but I think that someone had come up with the GGD already and there may be some prior researches.
If you have any research information about this model, please let me know.


## Package overview

This R package can generate objects for the following kinds of distribution models:

+ 0. normal distribution
+ 1. mean of 2 normal distributions (a kind of Gaussian mixture distribution)
+ 2. mixture of 2 normal distributions with gradually changing ratio along the horizontal (x-axis) direction (Horizontal Gradational Distribution)
+ 3. mixture of 2 or 3 normal distributions with gradually changing ratio along the vertical (y-axis) direction (Vertical Gradational Distribution)
+ 4. mixture of 4 normal distributions with gradually changing ratio along both the vertical (y-axis) and horizontal (x-axis) directions (Horizontal-Vertical Gradational Distribution)

The 0 and 1 of above are not kinds of GGD model, however,
they can be generated for the purpose of comparing distribution models.

Each of 1 to 4 of above can be further classified
according to the conditions of the normal distribution of the components as follows:

+ with different means and equal standard deviations (Mean-Differed-Sigma-Equaled Distribution)
+ with equal means and different standard deviations (Mean-Equaled-Sigma-Differed Distribution)
+ both means and standard deviations are different (Mean-Differed-Sigma-Differed Distribution)


## Major functions and members

| Type      | Name                  | Overview                                                  |
| :-------: | :-------------------: | :-------------------------------------------------------- |
| Generator | ggd.nls.freq          | Generates a GGD object that approximates a frequency distribution. |
| 〃        | ggd.nls.freq.all      | Approximates a frequency distribution with all supported distribution models. |
| 〃        | ggd.trace.q           | Generates a GGD object that traces quantiles.             |
| 〃        | ggd.set.cmp           | Generates a GGD object with indicated components.         |
| Field     | median                | The median value  of the distribution.                    |
| 〃        | mean                  | The mean value of the distribution.                       |
| 〃        | sd, usd, lsd          | The standard deviation, upper semi-standard deviation and lower semi-standard deviation. |
| RC Method | d                     | Returns the values of the probability density function.   |
| 〃        | p                     | Returns the values of the cummulative probability function.    |
| 〃        | q                     | Returns the values of the quantile function.              |
| 〃        | r                     | Returns random samples following the distribution.        |
| 〃        | tex                   | Displays the formulas of the probability density function and the cumulative distribution function in TeX format. |
| 〃        | read.csv              | Read the composition of a GGD object from a CSV file.     |
| 〃        | write.csv             | Write the composition of a GGD object to a CSV file.      |


## Installation

<pre>
# Install devtools from CRAN
install.packages( "devtools" )

# Then use devtools::install_github( "user/repository" ) to install cgd package from GitHub
devtools::install_github( "Kimitsuna-Goblin/ggd" )
</pre>


## Kinds of supported distribution models

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

When approximating a frequency distribution, if you get the result of a normal distribution,
please do not hesitate to apply the normal distribution as the model.

You can trace 2 quantiles (e.g., tertiles).


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

Meanwhile, it is allow to try tracing 3 or 4 quantiles
with the cumulative density function.


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

When tracing quantiles with the cumulative density function,
you can trace 3 or 4 quantiles.
For example, the quartiles for $p = 0.25, 0.5, 0.75$ can be traced
with very little error (as long as they are not overly distorted).

However, if you know the frequency distribution,
it is recommended to approximate the frequency distribution
rather than tracing quantiles.
This is because generally you cannot be sure that the original data can be properly modeled
by just tracing 3 or 4 quartiles.


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

3-2. Vertical gradation with 3 normal distributions (1 for head and 2 for left and right side tails)

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

When tracing quantiles with the cumulative density function,
you can trace from 3 to 6 quantiles.

However, this vertical gradational distribution
is not well suited for tracing equally spaced probability points (e.g. $p = 0.25, 0.5, 0.75$).
This distribution model is better suited for tracing quantiles near the head and near the tale
(i.e. points far and near from the mean).
For example, quantiles with $p = 0.1, 0.4, 0.6, 0.9$
can be traced with very little error (as long as they are not overly distorted).

If you want to trace quantiles for symmetrical distributions,
you can use quantiles biased to one side
(e.g. points with $p = 0.1, 0.4, 0.5$).


### 4. Horizontal-Vertical Gradational Distribution

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

When tracing quantiles with the cumulative density function,
you can trace from 5 to 8 quantiles.
For example, the quantile points at $p = 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9$
can be traced.

More than 8 quantiles cannot be traced with this package.
If you have more than 8 quantiles, make a frequency distribution and try ggd.nls.freq instead.
