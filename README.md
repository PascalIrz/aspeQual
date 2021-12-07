<!-- badges: start -->
[![R-CMD-check](https://github.com/PascalIrz/aspeQual/workflows/R-CMD-check/badge.svg)](https://github.com/PascalIrz/aspeQual/actions)
<!-- badges: end -->


aspeQual
====

Ce package regroupe un ensemble de fonctions pour la mise en qualité de la base Aspe.

Pour le moment il est focalisé sur les mesures individuelles (biométrie réalisée sur les poissons), en particulier :

- Distributions en taille (longueur), avec les fonctions préfixées par `qta_` 
- Relations taille - poids avec les fonctions préfixées par `qtp_` ([tuto](https://rpubs.com/kamoke/729779))

Installation
------------

Si besoin, commencer par installer le package R {devtools} ainsi que la suite Rtools, puis installer {aspe} au moyen de la commande :


    devtools::install_github("PascalIrz/aspeQual")

