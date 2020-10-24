
<!-- README.md is generated from README.Rmd. Please edit that file -->

# somar <img src="./man/figures/logo.png" align="right" width="160" />

<!-- badges: start -->

<!-- CI badge -->

<!-- Coverage badge -->

<!-- Version/Release badge -->

[![GPLv3
License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/licenses/)
<!-- badges: end -->

## Overview

The goal of somar (built on top of packages such as igraph, plyr,
stringr) is to offer specific data visualisation tools in the field of
neurology and more precisely (for the moment) concerning the motor
functions of the brain. The initiative was launched by one of the
members,
<a href="http://coactionslab.com/people/102-top-menu/people/current-members/168-gerard-derosiere" target="_blank">Gérard
Derosière</a>, of the
<a href="http://coactionslab.com/" target="_blank">CoActionsLab<a/>
research laboratory located in Brussels. The main idea behind the
construction of this package is to give anyone who so desires the
opportunity to come and enrich this library so that anyone doing
research in the field of neurology has the appropriate tools to express
their innovative ideas. So don’t hesitate to make a pull request.

## Installation

You can install the released and development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JeremyGillard/somar")
```

R version: 3.6.3

## Getting Started

It is possible to use this package in two different ways:

The first is to use functions that automate a large part of the work.
Their operation is based on a structure and a precise naming convention
for the columns of the dataframe entered.

See <a href="./vignettes/guide.md">Guide</a>

The second is to use each of the functions available in the dataframe
separately, in order to use them in a more personalised way.

See <a href="./vignettes/detailedGuide.md">Detailed Guide</a>

## License

somar is <a href="./LICENSE">GPL-3 licensed</a>
