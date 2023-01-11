# GGD -  Gradational Gaussian Distribution

## What is Gradational Gaussian Distribution? - Gradational Gaussian Distribution とは

Gradational Gaussian Distribution (漸変ガウス分布？) は、
主として単峰性の、正規分布に従わない分布をターゲットとした連続分布モデルです。

この分布モデルは、正規分布 (ガウス分布) の混合分布モデルの一種ですが、
いわゆる混合ガウス分布、すなわち正規分布の一次結合で表される分布モデルとは異なり、
X軸方向やY軸方向に沿って、次第に正規分布の混合比率を変化させた分布モデルです。
なお、正規分布の関数の畳み込みではありません。

## About this library - このライブラリの概要

このサイトの R 言語ライブラリでは、大きく分けて、以下の種類の分布モデルが生成できます。

0. 正規分布
1. 2つの正規分布の平均 (混合ガウス分布)
2. 横方向 (X軸方向) に2つの正規分布の混合比率が漸次的に変化する分布
3. 縦方向 (Y軸方向) に2つの正規分布の混合比率が漸次的に変化する分布
4. 縦横両方向に4つの正規分布の混合比率が漸次的に変化する分布

0. と 1. は Gradational Gaussian Distribution ではありませんが、
分布モデルの比較のために、生成できるようにしています。

上のそれぞれの大まかな分類は、
さらに、分布を構成する2つないし4つの正規分布の条件によって、

1. 平均値が異なり、標準偏差が等しい
2. 平均値が等しく、標準偏差が異なる
3. 平均値と標準偏差の両方が異なる

のように細分化されます。
直感的に分かるように、 4-3 の分布が、この中では最も自由度が高く、複雑な分布を表現できますが、
3-1 や 2-2 ぐらいの、より単純なモデルの方が、データを分析しやすいケースも多いと思われます。

Gradational Gaussian Distribution は、
連結ガウス分布 (Connected Gaussian Distribution; CGD) からの派生で[^1]、
一応、このリポジトリの著者が考案したものですが、
わりと誰でも思いつきそうな分布モデルだと思いますので、
もし、それ以前の情報があれば教えてください。

[^1]: 著者のローカル開発環境では、
まず先に、この Gradational Gaussian Distribution の一部があったのですが、
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

