
<!-- README.md is generated from README.Rmd. Please edit that file -->

# somar

<!--- <img src="man/figures/logo.png" align="right" width="120" /> --->

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

## Getting Started

It is possible to use this package in two different ways.

The first use is based on data structuring and a precise naming
convention for the variables. The second use is based on a precise
naming convention for the variables, thus allowing the use of few
functions that automate the whole graph construction.

See <a href="./vignettes/guide.md">Guide</a>

The second consists in using more precisely each of the functions
developed in this package in order to leave to the user the greatest
possible usability.

See <a href="./vignettes/detailedGuide.Rmd">Detailed Guide</a>

## License

somar is <a href="./LICENSE">GPL-3 licensed</a>

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(somar)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
