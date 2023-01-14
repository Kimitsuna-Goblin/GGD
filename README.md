# GGD -  Gradational Gaussian Distribution

このR言語パッケージは、度数分布の近似あるいはクォンタイルのトレースにより、
Gradational Gaussian Distribution のR5クラスオブジェクトを生成するものです。

[English README](README.en.md)


## Gradational Gaussian Distribution とは

Gradational Gaussian Distribution (漸変ガウス分布？) は、
主として単峰性の、正規分布に従わない分布をターゲットとした連続分布モデルです。

この分布モデルは、正規分布 (ガウス分布) の混合分布モデルの一種ですが、
いわゆる混合ガウス分布 (Gaussian Mixture Distribution)、すなわち、正規分布の一次結合で表される分布モデルとは異なり、
X軸方向やY軸方向に沿って、徐々に正規分布の混合比率を変化させた分布モデルです。
なお、正規分布の関数の畳み込みではありません。

この分布モデルは、
正規分布に従わないデータがあったとき、
その分布が正規分布に従わない原因が、クラスタリング可能な、離散的なパラメータによるものではなく、
何か連続的なパラメータによるものだった場合に、モデルとして適用できるだろうと、私は考えています。

この分布モデルは、
[連結ガウス分布 (Connected Gaussian Distribution; CGD)](https://github.com/Kimitsuna-Goblin/cgd) からの派生で[^1]、
一応、このパッケージの作者が考案したものですが、
わりと誰でも思いつきそうな分布モデルだと思いますので、
先人の研究があるんじゃないかろうか？と思っています。
もし、この分布モデルに関する、2021年以前の情報があれば教えてください。


## このパッケージの概要

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
3. 平均値と標準偏差の両方が異なる正規分布の混合 (Mean-Differed Sigma-Differed)

のように細分化されます。
これらの中では、 4-3 の分布が、最も自由度が高く、複雑な分布を表現できますが、
3-1 や 2-2 などの、より単純なモデルの方が、データを分析しやすいケースも多いと思われます。

[^1]: 作者のローカル開発環境では、
まず先に、この Gradational Gaussian Distribution の一部の確率密度関数があったのですが、
それは、理論的に整備した分布モデルではなく、直感的に作ってみた関数群でした。
そして、それに飽き足らず、クラス化して「連結ガウス分布」を考案してみたところ、
結局それを突き詰めたら、「Gradational Gaussian Distribution」に戻ってきたのでした。


## 主な機能

| 種別          | 関数名            | 機能                                                              |
| :-----------: | :---------------: | :---------------------------------------------------------------- |
| ジェネレータ  | nls.freq          | 度数分布を近似する GGD クラスオブジェクトを生成します。           |
| 〃            | nls.freq.all      | サポートしている全種類の分布モデルで度数分布の近似を試みます。    |
| 〃            | trace.q           | クォンタイルをトレースするGGD クラスオブジェクトを生成します。    |
| メソッド      | $d                | 確率密度関数の値を返します。                                      |
| 〃            | $p                | X座標 (クォンタイル) に対する確率を返します。                     |
| 〃            | $q                | 確率に対するX座標 (クォンタイル) を返します。                     |
| 〃            | $r                | 分布モデルに従うランダムサンプルを返します。                      |
| 〃            | $sd, $usd, $lsd   | 標準偏差、上側標準偏差、下側標準偏差を返します。                  |
| 〃            | $tex              | 確率密度関数と累積分布関数を TeX 形式で表示します。               |


## インストール

<pre>
# Install devtools from CRAN
install.packages( "devtools" )

# Then use devtools::install_github( "user/repository" ) to install cgd package from GitHub
devtools::install_github( "Kimitsuna-Goblin/ggd" )
</pre>

## 分布の種類

### 0. 正規分布

#### 確率密度関数・累積分布関数

確率密度関数 $f(x)$ と累積分布関数 $\Phi(x)$ は以下の式で表されます。

$$
\begin{align}
f(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \exp \left( -\dfrac{(x - \mu)^2}{2 \sigma^2} \right)\\
\Phi(x) &= \dfrac{1}{\sqrt{2 \pi \sigma^2}} \int_{-\infty}^{x} \exp \left( -\dfrac{(t - \mu)^2}{2 \sigma^2} \right) dt
\end{align}
$$

#### 解説

いわゆる正規分布です。

#### パッケージにおける "kind" の名前

+ Normal Distribution


### 1. 2つの正規分布の平均

#### 確率密度関数・累積分布関数

確率密度関数 $g(x)$ と累積分布関数 $\Psi(x)$ は
2つの正規分布の確率密度関数 $f_1(x), f_2(x)$
と累積分布関数 $\Phi_1(x), \Phi_2(x)$ を使って、以下の式で表されます。

$$
\begin{align}
g(x) &= \dfrac{1}{2} ( f_1(x) + f_2(x) )\\
\Psi(x) &= \dfrac{1}{2} ( \Phi_1(x) + \Phi_2(x) )
\end{align}
$$

#### 解説

このクラスは Gradational Gaussian Distribution との違いを確認するためにサポートしている、混合ガウス分布の例です。

混合ガウス分布は本来、単峰性でない分布や、正規分布に従わない分布のデータを
正規分布に従う複数のクラスターデータに分割するための手段です。

#### パッケージにおける "kind" の名前

+ Mean of Mean-Differed Sigma-Equaled 2 Normal Distributions
+ Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions
+ Mean of Mean-Differed Sigma-Differed 2 Normal Distributions


### 2. 横方向グラデーション分布

#### 確率密度関数・累積分布関数

確率密度関数 $g(x)$ と累積分布関数 $\Psi(x)$ は
2つの正規分布の確率密度関数 $f_1(x), f_2(x)$
と累積分布関数 $\Phi_1(x), \Phi_2(x)$ を使って、以下の式で表されます。

$$
\begin{align}
g(x) &= \left( 1 - \Phi_1(x) \right) f_1(x) + \Phi_2(x) f_2(x)\\
\Psi(x) &= \Phi_1(x) - \dfrac{1}{2} \Phi_1(x)^2 + \dfrac{1}{2} \Phi_2(x)^2
\end{align}
$$

#### 解説

このクラスでは、
$x$ が $-\infty \to \infty$ と増加するにつれて、
正規分布の確率密度関数 $f_1(x)$ の負担率は $1 \to 0$ と減少し、
$f_2(x)$ の負担率は逆に $0 \to 1$ と増加します。

累積密度関数でクォンタイルをトレースする場合は、3点または4点のクォンタイルをトレースできます。
例えば、四分位点 $\lbrace 0.25, 0.5, 0.75 \rbrace$ を
 (過度にいびつな形でなければ) ほとんど誤差なくトレースできます。

ただし、
度数分布が分かっている場合は、
四分位点をトレースさせるよりも、
度数分布を入力して、それに近い分布を算出させることをお勧めします。
四分位点のトレースだけでは、
元データを適切にモデル化できるかどうか、分からないからです。


#### パッケージにおける "kind" の名前

+ Mean-Differed Sigma-Equaled Horizontal Gradational Distribution
+ Mean-Equaled Sigma-Differed Horizontal Gradational Distribution
+ Mean-Differed Sigma-Differed Horizontal Gradational Distribution


### 3. 縦方向グラデーション分布

#### 確率密度関数・累積分布関数

確率密度関数 $g(x)$ と累積分布関数 $\Psi(x)$ は
2つ、あるいは3つの正規分布の確率密度関数 $f_i(x)$
と累積分布関数 $\Phi_i(x)$ $(i = 1, 2, 3)$ を使って、以下の式で表されます。

以下の式において、
$\Phi^\ast_i(x)$ は平均値が $\mu_i$ で標準偏差が $\displaystyle \frac{\sigma_i}{\sqrt{2}}$
の正規分布の累積分布関数を表します。

3-1. 2つの正規分布のグラデーション (山側1個と裾側1個)

$$
\begin{align}
g(x) &= \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) + \dfrac{f_2(x)}{f_2(\mu_2)} f_2(x)\\
\Psi(x) &= \Phi_1(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_1(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_2(x)
\end{align}
$$

3-2. 3つの正規分布のグラデーション (山側1個と裾側2個 (左右各1個))

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

#### 解説

このクラスでは、
正規分布の確率密度関数 $f_i(x)$ $(i = 1, 2, 3)$ の値が
$0 \to f_i(\mu_i)$ と大きくなるにつれて、
$f_1(x)$ と $f_3(x)$ の負担率は $1 \to 0$ と減少し、
$f_2(x)$ の負担率は $0$ から徐々に増加していきます。

正規分布の裾に近いほど、 $f_1(x)$ と $f_3(x)$ の負担率が大きくなり、
山の頂点に近いほど、 $f_2(x)$ の負担率が大きくなるイメージです。

標準偏差 $\sigma_i$ $(i = 1, 2, 3)$ については、
山側の標準偏差 $\sigma_2$ よりも、裾側の標準偏差 $\sigma_1, \sigma_3$ のほうが、
より大きい値であるべきです。

累積密度関数でクォンタイルをトレースする場合は、最大6点のクォンタイルをトレースできます。

ただし、この縦方向グラデーション分布は、等間隔の四分位数のトレースには、あまり適しません。
この分布モデルは、山部と裾部の代表点、つまり、平均値に近い点と遠い点のトレースに適します。
例えば、確率 $\lbrace 0.1, 0.4, 0.5, 0.6, 0.9 \rbrace$ のようなクォンタイルを
 (過度にいびつな形でなければ) ほとんど誤差なくトレースできます。

左右対称な分布のクォンタイルをトレースする場合は、
確率 $\lbrace 0.1, 0.4, 0.5 \rbrace$ のように、
一方の側に偏ったクォンタイルも有効です。

#### パッケージにおける "kind" の名前

3-1. 2つの正規分布のグラデーション (山側1個と裾側1個)
+ Mean-Differed Sigma-Equaled Vertical Gradational Distribution
+ Mean-Equaled Sigma-Differed Vertical Gradational Distribution
+ Mean-Differed Sigma-Differed Vertical Gradational Distribution


3-2. 3つの正規分布のグラデーション (山側1個と裾側2個 (左右各1個))
+ 3-Mean-Differed Sigma-Equaled Vertical Gradational Distribution
+ Mean-Equaled 3-Sigma-Differed Vertical Gradational Distribution
+ 3-Mean-Differed 3-Sigma-Differed Vertical Gradational Distribution


### 4. 縦横グラデーション分布

#### 確率密度関数・累積分布関数

確率密度関数 $g(x)$ と累積分布関数 $\Psi(x)$ は
4つの正規分布の確率密度関数 $f_{i,j}(x)$
と累積分布関数 $\Phi_{i,j}(x)$ $(i, j = 1, 2)$を使って、以下の式で表されます。

以下の式において、 $\Phi^\ast_{i,j}(x)$ は平均値が $\mu_{i,j}$ で標準偏差が $\displaystyle \frac{ \sigma_{i,j} }{ \sqrt{2} }$ の正規分布の累積分布関数を表します。

$$
\begin{align}
g(x) &= \left( 1 - \Psi_1(x) \right) g_1(x) + \Psi_2(x) g_2(x)\\
\Psi(x) &= \Psi_1(x) - \dfrac{1}{2} \Psi_1(x)^2 + \dfrac{1}{2} \Psi_2(x)^2\\
\\
g_i(x) &= \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right) f_{i,1}(x) + \dfrac{f_{i,2}(x)}{f_{i,2}(\mu_{i,2})} f_{i,2}(x)\\
\Psi_i(x) &= \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,1}(x) + \dfrac{1}{\sqrt{2}} \Phi^\ast_{i,2}(x)
\end{align}
$$

#### 解説

このクラスでは、確率密度関数 $g(x)$ は
4つの正規分布の確率密度関数 $f_{1,1}(x), f_{1,2}(x), f_{2,1}(x), f_{2,2}(x)$ によって負担されます。

$f_{1,1}(x)$ は分布の左側 ( $x$ が平均値よりも小さい方) の裾側、 $f_{1,2}(x)$ は分布の左側の山側、
$f_{2,1}(x)$ は分布の右側 ( $x$ が平均値よりも大きい方) の裾側、 $f_{2,2}(x)$ は分布の右側の山側を
それぞれ主に負担します。

本パッケージの分布モデルの中では、最も自由度が高く、最も複雑な分布を表現できます。

累積密度関数でクォンタイルをトレースする場合は、最大8点のクォンタイルをトレースできます。
例えば、確率 $\lbrace 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 \rbrace$ のクォンタイルを
 (過度にいびつな形でなければ) ほとんど誤差なくトレースできます。

9点以上のクォンタイルをトレースしたい場合は、
影響の少なそうなクォンタイルを除外して8点以下にするか、
[連結ガウス分布 (Connected Gaussian Distribution; CGD)](https://github.com/Kimitsuna-Goblin/cgd)
の不連続分布をご利用ください。

#### パッケージにおける "kind" の名前

+ Mean-Differed Sigma-Equaled Vertical-Horizontal Gradational Distribution
+ Mean-Equaled Sigma-Differed Vertical-Horizontal Gradational Distribution
+ Mean-Differed Sigma-Differed Vertical-Horizontal Gradational Distribution

