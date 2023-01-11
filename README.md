# GGD -  Gradational Gaussian Distribution

## What is Gradational Gaussian Distribution? - Gradational Gaussian Distribution �Ƃ�

Gradational Gaussian Distribution (�Q�σK�E�X���z�H) �́A
��Ƃ��ĒP�����́A���K���z�ɏ]��Ȃ����z���^�[�Q�b�g�Ƃ����A�����z���f���ł��B

Gradational Gaussian Distribution �́A���K���z (�K�E�X���z) �̍������z���f���̈��ł����A
�����鍬���K�E�X���z (Gaussian Mixture Model)�A���Ȃ킿�A���K���z�̈ꎟ�����ŕ\����镪�z���f���Ƃ͈قȂ�A
X��������Y�������ɉ����āA����ɐ��K���z�̍����䗦��ω����������z���f���ł��B
�Ȃ��A���K���z�̊֐��̏�ݍ��݂ł͂���܂���B

Gradational Gaussian Distribution �́A
[�A���K�E�X���z (Connected Gaussian Distribution; CGD)](https://github.com/Kimitsuna-Goblin/cgd) ����̔h����[^1]�A
�ꉞ�A���̃��C�u�����̍�҂��l�Ă������̂ł����A
���ƒN�ł��v���������ȕ��z���f�����Ǝv���܂��̂ŁA
�����A����Ɋւ���A2021�N�ȑO�̏�񂪂���΋����Ă��������B

## About this library - ���̃��C�u�����̊T�v

���̃T�C�g�� R ���ꃉ�C�u�����ł́A�傫�������āA�ȉ��̎�ނ̕��z���f���������ł��܂��B

0. ���K���z (Normal Distribution)
1. 2�̐��K���z�̕��� (Mean of 2 Normal Distributions) (�����K�E�X���z; Gaussian Mixture Model)
2. ������ (X������) ��2�̐��K���z�̍����䗦���Q���I�ɕω����镪�z (Horizontal Gradational Distribution)
3. �c���� (Y������) ��2�܂���3�̐��K���z�̍����䗦���Q���I�ɕω����镪�z (Vertical Gradational Distribution)
4. �c����������4�̐��K���z�̍����䗦���Q���I�ɕω����镪�z (Vertical-Horizontal Gradational Distribution)

��� 0. �� 1. �� Gradational Gaussian Distribution �ł͂���܂��񂪁A
���z���f���̔�r�̂��߁A�����ł���悤�ɂ��Ă��܂��B

��̂��ꂼ��̑�܂��ȕ��ނ́A����ɁA�\���v�f�̐��K���z�̏����ɂ���āA

1. ���ϒl���قȂ�A�W���΍������������K���z�̍��� (Mean-Differed Sigma-Equaled)
2. ���ϒl���������A�W���΍����قȂ鐳�K���z�̍��� (Mean-Equaled Sigma-Differed)
3. ���ϒl�ƕW���΍��̗������قȂ鐳�K���z�̍��� (Mean-Differd Sigma-Differed)

�̂悤�ɍו�������܂��B
�����̒��ł́A 4-3 �̕��z���A�ł����R�x�������A���G�ȕ��z��\���ł��܂����A
3-1 �� 2-2 �Ȃǂ́A���P���ȃ��f���̕����A�f�[�^�𕪐͂��₷���P�[�X�������Ǝv���܂��B

[^1]: ��҂̃��[�J���J�����ł́A
�܂���ɁA���� Gradational Gaussian Distribution �̈ꕔ�̊m�����x�֐����������̂ł����A
����́A���_�I�ɐ����������z���f���ł͂Ȃ��A�����I�ɍ���Ă݂��֐��Q�ł����B
�����āA����ɖO�����炸�A�N���X�����āu�A���K�E�X���z�v���l�Ă��Ă݂��Ƃ���A
���ǂ����˂��l�߂���A�uGradational Gaussian Distribution�v�ɖ߂��Ă����̂ł����B

## Installation - �C���X�g�[��

<pre>
# Install devtools from CRAN
install.packages( "devtools" )

# Then use devtools::install_github( "user/repository" ) to install cgd package from GitHub
devtools::install_github( "Kimitsuna-Goblin/ggd" )
</pre>

