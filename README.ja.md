# GGD -  Gradational Gaussian Distribution

これは Gradational Gaussian Distribution (GGD) のリファレンスクラスの R パッケージです。
GGD によって度数分布を近似したり、分位点をトレースすることができます。

[English](README.md)


## Gradational Gaussian Distribution とは

Gradational Gaussian Distribution (GGD; 漸次的ガウス分布 あるいは グラデーションガウス分布) は、
主として単峰性の、正規分布に従わない分布をターゲットとした連続分布モデルの一つです。
正規分布に従わない歪んだ分布、裾の重い分布、頂点が扁平な分布などを表すことができます。

GGD は正規分布 (ガウス分布) の混合分布モデルの一種ですが、
いわゆる混合ガウス分布 (Gaussian Mixture Model; GMM)、すなわち、正規分布の一次結合で表されるモデルとは異なり、
X軸方向やY軸方向に沿って、正規分布の混合比率を徐々に変化させた分布モデルです。
なお、正規分布の関数の畳み込みではありません。

GGD はデータが正規分布に従わないとき、その原因が、離散的なパラメータによるものではなく、
何か連続的なパラメータによる場合に適用できるだろうと、パッケージ作者の私は考えています。

### 横方向グラデーション分布 (Horizontal Gradational Distribution)

**横方向グラデーション分布 (Horizontal Gradational Distribution)** は
2つの正規分布の混合比がX軸に沿って徐々に変化する分布モデルです。
左右に歪んだ分布を表すのに適しています。 

X座標が $-\infty \to \infty$ と漸増するにつれて、確率密度関数が
正規分布 $\mathcal{N}_1$ のものから$正規分布 $\mathcal{N}_2$ のものに徐々に変化するような
横方向グラデーション分布を $\mathcal{G}[\mathcal{N}_1 \rightarrow \mathcal{N}_2]$ と書くことにします。

一般に $\mathcal{G}[\mathcal{N}_1 \rightarrow \mathcal{N}_2]$ は次のように定義できます。

$$
\mathcal{G}[\mathcal{N}_1 \rightarrow \mathcal{N}_2] =
    h_1(x) \ \mathcal{N}_1 + h_2(x) \ \mathcal{N}_2
$$

ここで、$h_1$ はX軸に沿って
$x:-\infty \to \infty \ \Rightarrow \ h_1(x):1 \to to 0$ のように徐々に減少する混合比を表し、
$h_2$ は $x:-\infty \to \infty \  \ \Rightarrow \ h_2(x):0 \to 1$ のように
徐々に増加する混合比を表します。

なお、 $h_1(x) + h_2(x)$ は、常に $1$ である必要はありません。
ただし、 $\forall x, h_i(x) \in [0,1]$ $(i = 1,2)$ と
$\lim_{x \to -\infty} h_1(x) + h_2(x) = \lim_{x \to \infty} h_1(x) + h_2(x) = 1$ は成り立つべきです。

X軸の左側（下側）では $\mathcal{N}_1$ が支配的であり、
右側 (上側) では $\mathcal{N}_2$ が支配的なので、
$\mathcal{N}_1$を**左側 (下側) 分布**、$\mathcal{N}_2$を**右側 (上側) 分布** と呼ぶことにします。

本パッケージでは、$h_1(x) = 1 - \Phi_1(x)$ と $h_2(x) = \Phi_2(x)$ を用います。
なお $\Phi_1$ と $\Phi_2$ は、それぞれ正規分布 $\mathcal{N}_1$ と $\mathcal{N}_2$ の累積分布関数です。

### 縦方向グラデーション分布 (Vertical Gradational Distribution) (2-コンポーネント)

**縦方向グラデーション分布 (Vertical Gradational Distribution)** は
2つの正規分布の混合比がY軸に沿って徐々に変化する分布モデルです。
裾の重い分布や頂点が扁平な分布を表すのに適しています。 

縦方向グラデーション分布を $\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2]$ のように書くことにします。
この記号は、少なくともイメージとして、
Y座標が $0 \to 1$ と漸増するにつれて、確率密度関数が
正規分布 $\mathcal{N}_1$ のものから$正規分布 mathcal{N}_2$ のものに徐々に変化することを表しています。
このとき、$\mathcal{N}_1$ を **裾側分布**、$\mathcal{N}_2$ を **山側分布** と呼ぶことにします。

一般に $\mathcal{G}[\mathcal{N}_1  \uparrow \mathcal{N}_2]$ は次のように定義できます。

$$
\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2] =
    v_1(x) \ \mathcal{N}_1 + v_2(x) \ \mathcal{N}_2
$$

ここで、混合比 $v_1$ と $v_2$ については、
正規分布 $\mathcal{N}_1$ と $\mathcal{N}_2$ の確率密度関数 $f_1$ と $f_2$ に対して、

$$
f_1(x):0 \to \max_{x \in (-\infty, \infty)}f_1(x)
    \ \Rightarrow \ v_1(x):1 \to 0
$$

$$
f_2(x):0 \to \max_{x \in (-\infty, \infty)}f_2(x)
    \ \Rightarrow \ v_2(x):0 \to 1
$$

が成り立ち、
$\mathcal{N}_1$ and  $\mathcal{N}_2$ の平均値 $\mu_1$ と $\mu_2$ に対しては、

$$
x:-\infty \to \mu_1 \ \Rightarrow \ v_1(x):1 \to 0, \ \ \ \
x:\mu_1 \to \infty \ \Rightarrow \ v_1(x):0 \to 1
$$

$$
x:-\infty \to \mu_2 \ \Rightarrow \ v_2(x):0 \to 1, \ \ \ \
x:\mu_2 \to \infty \ \Rightarrow \ v_2(x):1 \to 0
$$

が成り立つものとします。

横方向グラデーションと同様に、 $v_1(x) + v_2(x)$ は、常に $1$ である必要はありませんが、
$\forall x, v_i(x) \in [0,1]$ $(i = 1,2)$ と
$\lim_{x \to -\infty} v_1(x) + v_2(x) = \lim_{x \to \infty} v_1(x) + v_2(x) = 1$ は成り立つべきです。

本パッケージでは、$v_1(x) = 1 - f_1(x) / f_1(\mu_1)$
と $v_2(x) = f_2(x) / f_2(\mu_1)$ を用います。

### 縦方向グラデーション分布 (Vertical Gradational Distribution) (3-コンポーネント)

縦方向グラデーション分布の裾側は、X軸に沿って左側（下側）と右側（上側）に分けることができます。

つまり、x座標が分布の $-\infty$ 側の裾から山に向かうにつれて、
確率密度関数が$\mathcal{N}_1$のものから$\mathcal{N}_2$のものに徐々に変化し、
また頂点から $\infty$ 側の裾に向かうにつれて、
今度は確率密度関数が$\mathcal{N}_3$のものに変化していくような分布モデルを考えることができます。

そのような分布モデルを
$\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2 \downarrow \mathcal{N}_3]$
のように表すことにします。
この分布モデルは一般に、次のように定義できます。

$$
\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2 \downarrow \mathcal{N}_3] =
    v_1(x) \ \mathcal{N}_1 + v_2(x) \ \mathcal{N}_2 + v_3(x) \ \mathcal{N}_3
$$

ただし、$v_1$ と $v_3$ は次のように漸増あるいは漸減する混合比です。

$$
x:-\infty \to \mu_1 \ \Rightarrow \ v_1(x):1 \to 0, \ \ \ \
    v_1(x) = 0 \ \ \mathrm{where} \ \ x > \mu_1
$$

$$
v_3(x) = 0 \ \ \mathrm{where} \ \ x < \mu_3, \ \ \ \
    x:\mu_3 \to \infty \ \Rightarrow \ v_3(x):0 \to 1.
$$

また、$v_2(x)$ は 2-コンポーネントの場合と同様に定義される混合比です。

なお、$\mathcal{N}_1$ は **左裾側 (下裾側) 分布**、
$\mathcal{N}_3$ は **右裾側 (上裾側) 分布** と呼ぶことにします。
ただし、この呼び方は、それぞれの平均値 $\mu_1$ と $\mu_3$ のどちらが大きくても構わないものとします。

### 横-縦グラデーション分布 (Horizontal-Vertical Gradational Distribution)

2つの縦方法グラデーション分布 $\mathcal{G}_1$ と $\mathcal{G}_2$ を
横方向グラデーション分布のように混合させることができます。

そのような分布を
$\mathcal{G}[\mathcal{G}_1 \rightarrow \mathcal{G}_2]$
のように表すことにします。
この分布モデルは

$$
\mathcal{G}[\mathcal{G}_1 \rightarrow \mathcal{G}_2] =
    h_1(x) \ \mathcal{G}_1 + h_2(x) \ \mathcal{G}_2
$$

のように定義できます。
ただし、$h_1(x)$ と $h_2(x)$ は横方向グラデーション分布の混合比と同様です。

$\mathcal{G}_1$ と $\mathcal{G}_2]$ は次のように定義される分布モデルです
(ごめんなさい。 $\mathcal{G}$ 、$\mathcal{N}$ の代わりに $G$ 、$N$ と書いています。
 GitHub のレンダリングエンジンは添字2つの $\mathcal{G}$ や $\mathcal{N}$ が複数あると、
 どうもうまく式を解釈してくれないようなので ( ˘•ω•˘ ))。

$$
G_1 = G[N_{1,1} \uparrow N_{1,2}]
  = v_{1,1}(x) \ N_{1,1} + v_{1,2}(x) \ N_{1,2},
$$

$$
G_2 =
    G[N_{2,1} \uparrow N_{2,2}] =
    v_{2,1}(x) \ N_{2,1} + v_{2,2}(x) \ N_{2,2}
$$

ただし、$N_{i,j}$ は正規分布で、
$v_{i,1}(x)$ 、 $v_{i,2}(x)$ は縦方向グラデーション分布の $v_1(x)$ 、$v_2(x)$ と同様の混合比です。
この分布モデルは例えば、左右に歪んでいて、かつ裾が重い分布などに適用できます。


## このパッケージの概要

このパッケージでは、大きく分けて、以下の種類の分布モデルが生成できます。

0. 正規分布 (Normal Distribution)
1. 2つの正規分布の平均 (Mean of 2 Normal Distributions) (混合ガウス分布)
2. 横方向グラデーション分布 (Horizontal Gradational Distribution)
3. 縦方向グラデーション分布 (Vertical Gradational Distribution) (2および3-コンポーネント)
4. 横-縦グラデーション分布 (Horizontal-Vertical Gradational Distribution)

0. と 1. はグラデーションガウス分布ではありませんが、
分布モデルの比較検討のために、生成できるようにしています。

上の大まかな分類は、コンポーネントの正規分布の条件によって

1. 平均値が異なり、標準偏差が等しい分布の混合 (Mean-Differed Sigma-Equaled)
2. 平均値が等しく、標準偏差が異なる分布の混合 (Mean-Equaled Sigma-Differed)
3. 平均値と標準偏差の両方が異なる分布の混合 (Mean-Differed Sigma-Differed)

のように、それぞれ細分化させています。
そのため、分布モデルの種類は全部で16種類あります。

番号が大きいほど自由度が高く、より複雑な分布を表現できますが、
データを分析するには、単純なモデルの方がやりやすいかも知れません。


## 主な機能

| 種別          | 関数名                | 機能                                                          |
| :-----------: | :-------------------: | :------------------------------------------------------------ |
| ジェネレータ  | ggd.nls.freq          | 度数分布を近似する GGD クラスオブジェクトを生成します。       |
| 〃            | ggd.nls.freq.all      | サポートしている全種類の分布で度数分布の近似を試みます。      |
| 〃            | ggd.trace.q           | 分位点をトレースする GGD クラスオブジェクトを生成します。     |
| 〃            | ggd.set.cmp           | GGD の構成を指定して、オブジェクトを生成します。              |
| フィールド    | median                | 分布の中央値です。                                            |
| 〃            | mean                  | 分布の平均値です。                                            |
| 〃            | sd, usd, lsd          | 分布の標準偏差、上側半標準偏差、下側半標準偏差です。          |
| RCメソッド    | d                     | 確率密度関数の値を返します。                                  |
| 〃            | p                     | X座標 (クォンタイル) に対する確率を返します。                 |
| 〃            | q                     | 確率に対するX座標 (クォンタイル) を返します。                 |
| 〃            | r                     | 分布に従うランダムサンプルを返します。                        |
| 〃            | tex                   | 確率密度関数と累積分布関数を TeX 形式で表示します。           |
| 〃            | read.csv              | GGD の構成をCSVファイルから読み込みます。                     |
| 〃            | write.csv             | GGD の構成をCSVファイルに保存します。                         |

平均値や標準偏差は R 標準の dnorm 関数および pnorm 関数と四則演算により算出しています
 (横-縦グラデーション分布の上下半標準偏差は数値積分による)。
したがって、計算精度は dnorm 関数や pnorm 関数の精度に依存します。

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

このクラスは、いわゆる正規分布です。

度数分布を近似したときに、結果が正規分布になることがあります。
その場合は、迷わず正規分布をモデルとして適用してください。

2点の分位点 (三分位点など) のトレースができます (2点でトレースと言えるのだろうか？)。

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

このクラスは混合ガウス分布の例です。
対象のデータに混合ガウス分布を適用すべきか、
それとも Gradational Gaussian Distribution を適用すべきかの判断材料としてお使いください。


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

累積密度関数で分位点をトレースする場合は、3点または4点の分位点をトレースできます。
例えば、 $p = 0.25, 0.5, 0.75$ の四分位点をほとんど誤差なくトレースできます。


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
$f_2(x)$ の負担率は $0$ からその最大値 ($1$ よりも小さいことがあります) に向かって、
徐々に増加していきます。

正規分布の裾に近いほど、 $f_1(x)$ と $f_3(x)$ の負担率が大きくなり、
山の頂点に近いほど、 $f_2(x)$ の負担率が大きくなるイメージです。

標準偏差 $\sigma_i$ $(i = 1, 2, 3)$ については、
$\sigma_2 \leq \sigma_1, \sigma_3$ となるのが望ましいです。
つまり、標準偏差は、山側よりも裾側のほうが大きい値であるべきです。
逆に、山側の方が大きいと、山が裾野の上に乗りきらず、裾野が山を「突き破って」しまいます。

累積密度関数で分位点をトレースする場合は、3点から6点までの分位点をトレースできます。

ただし、この縦方向グラデーション分布は、
$p = 0.25, 0.5, 0.75$ となる等間隔な四分位点のトレースなどには、あまり適していません。
この分布モデルは、山部と裾部の代表点、つまり、平均値に近い点と遠い点のトレースに適します。
例えば、 $p = 0.1, 0.4, 0.6, 0.9$ のような分位点を
 (過度にいびつな形でなければ) ほとんど誤差なくトレースできます。
左右対称な分布の分位点をトレースする場合は、
$p = 0.1, 0.4, 0.5$ のように、
一方の側に偏った分位点も有効です。


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

累積密度関数で分位点をトレースする場合は、5点から8点までの分位点をトレースできます。
例えば、 $p = 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9$ の分位点をトレース可能です。

9点以上の分位点は本パッケージの機能ではトレースできません。
分位点が9点以上ある場合は、度数分布を作成して、 ggd.nls.freq による近似をお試しください。


#### 終わりに

この分布モデルは、
[連結ガウス分布 (Connected Gaussian Distribution; CGD)](https://github.com/Kimitsuna-Goblin/cgd) からの派生で、
一応、このパッケージの作者が考案したものですが、
わりと誰でも思いつきそうな分布モデルだと思いますので、
先人の研究があるんじゃないかろうか？と思っています。
もし、この分布モデルに関する、2021年以前の情報があれば、教えてください。
私 (パッケージの作者) は一応、大学の数学科にいたことはありますが、
統計学の専門家じゃありませんので、そういう情報には疎いのです (^^;。

