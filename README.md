# tsvr: timescale-specific variance ratio

Lei Zhao, China Agricultural University
Shaopeng Wang, Peking University
Daniel C. Reuman, University of Kansas - maintainer 

This repository provides tools for timescale decomposition of the classic variance ratio of community ecology. Tools are as described in Zhao et al (in prep), extending commonly used methods introduced by Peterson et al (1975) <doi: 10.2307/1936306>.

## Explanations

In addition to the documentation, see the vignette, which is the best way to get started.

## Installation

The package is on the Comprehensive R Archive Network (CRAN).

For a quick install of this development version: devtools::install_github(repo="reumandc/tsvr"). The 
install_github function, by default, will not install the vignette. There is 
an option for installing vignettes (build_vignettes=TRUE), but it seems 
finicky. You can also clone the repository and use devtools::install() 
to install from your local clone. The build_vignettes=TRUE option seems 
to work better for devtools::install than it does 
for devtools::install_github. If you have trouble, please email me.
