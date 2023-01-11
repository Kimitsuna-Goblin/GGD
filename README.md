# GGD -  Gradational Gaussian Distribution

## What is Gradational Gaussian Distribution? - Gradational Gaussian Distribution とは

Gradational Gaussian Distribution (漸変ガウス分布？) は、
主として単峰性の、正規分布に従わない分布をターゲットとした連続分布モデルです。

Gradational Gaussian Distribution は、正規分布 (ガウス分布) の混合分布モデルの一種ですが、
いわゆる混合ガウス分布 (Gaussian Mixture Distribution)、すなわち、正規分布の一次結合で表される分布モデルとは異なり、
X軸方向やY軸方向に沿って、次第に正規分布の混合比率を変化させた分布モデルです。
なお、正規分布の関数の畳み込みではありません。

Gradational Gaussian Distribution は、
[連結ガウス分布 (Connected Gaussian Distribution; CGD)](https://github.com/Kimitsuna-Goblin/cgd) からの派生で[^1]、
一応、このパッケージの作者が考案したものですが、
わりと誰でも思いつきそうな分布モデルだと思いますので、
もし、これに関する、2021年以前の情報があれば教えてください。

## About this package - このパッケージの概要

このサイトの R 言語パッケージでは、大きく分けて、以下の種類の分布モデルが生成できます。

0. 正規分布 (Normal Distribution)
1. 2つの正規分布の平均 (Mean of 2 Normal Distributions) (混合ガウス分布; Gaussian Mixture Distribution)
2. 横方向 (X軸方向) に2つの正規分布の混合比率が漸次的に変化する分布 (Horizontal Gradational Distribution)
3. 縦方向 (Y軸方向) に2つまたは3つの正規分布の混合比率が漸次的に変化する分布 (Vertical Gradational Distribution)
4. 縦横両方向に4つの正規分布の混合比率が漸次的に変化する分布 (Vertical-Horizontal Gradational Distribution)

上の 0. と 1. は Gradational Gaussian Distribution ではありませんが、
分布モデルの比較のため、生成できるようにしています。

上のそれぞれの大まかな分類は、さらに、構成要素の正規分布の条件によって、

1. 平均値が異なり、標準偏差が等しい正規分布の混合 (Mean-Differed Sigma-Equaled)
2. 平均値が等しく、標準偏差が異なる正規分布の混合 (Mean-Equaled Sigma-Differed)
3. 平均値と標準偏差の両方が異なる正規分布の混合 (Mean-Differd Sigma-Differed)

のように細分化されます。
これらの中では、 4-3 の分布が、最も自由度が高く、複雑な分布を表現できますが、
3-1 や 2-2 などの、より単純なモデルの方が、データを分析しやすいケースも多いと思われます。

[^1]: 作者のローカル開発環境では、
まず先に、この Gradational Gaussian Distribution の一部の確率密度関数があったのですが、
それは、理論的に整備した分布モデルではなく、直感的に作ってみた関数群でした。
そして、それに飽き足らず、クラス化して「連結ガウス分布」を考案してみたところ、
結局それを突き詰めたら、「Gradational Gaussian Distribution」に戻ってきたのでした。

## Installation - インストール

<pre>
# Install devtools from CRAN
install.packages( "devtools" )

# Then use devtools::install_github( "user/repository" ) to install cgd package from GitHub
devtools::install_github( "Kimitsuna-Goblin/ggd" )
</pre>

## Kinds of distributions - 分布の種類

### 0. Normal Distribution - 正規分布

#### $f(x)$ : Distribution Function and $\Phi(x)$ : Cumulative Distribution Function - 確率密度関数 $f(x)$ ・累積分布関数 $\Phi(x)$

$$
\begin{align}
f(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma^2} \right)\\
\Phi(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma^2} \right) dt
\end{align}
$$

#### Description - 解説

いわゆる正規分布です。

#### Names of "kind" at this package - パッケージにおける "kind" の名前

+ Normal Distribution


### 1. Mean of 2 Normal Distributions - 2つの正規分布の平均

#### $g(x)$ : Distribution Function and $\Psi(x)$ : Cumulative Distribution Function - 確率密度関数 $g(x)$ ・累積分布関数 $\Psi(x)$

$$
\begin{align}
g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) )\\
\Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) )\\
\\
f_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma_i^2} \right)\\
\Phi_i(x) &= \dfrac{1}{\sqrt{2 \pi \sigma_i^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma_i^2} \right) dt
\end{align}
$$

#### Description - 解説

Gradational Gaussian Distribution との違いを確認するためにサポートしている、混合ガウス分布の例です。

混合ガウス分布は本来、単峰性でない分布や、正規分布に従わない分布のデータを
正規分布に従う複数のクラスターデータに分割するための手段です。

平均値が異なる正規分布の平均と、標準偏差が異なる正規分布の平均が生成できますが、
平均値が異なる2つの正規分布の平均は、単峰性分布のモデルとしては、適切でないかも知れません。

#### Names of "kind" at this package - パッケージにおける "kind" の名前

+ Mean of Mean-Differed Sigma-Equaled 2 Normal Distributions
+ Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions
+ Mean of Mean-Differed Sigma-Differed 2 Normal Distributions

